---
name: dx-data
description: >
  DX.Data micro-ORM for Delphi — schema from entity attributes, validation,
  mapping, migrations, and FireDAC data modules. Trigger when: creating or
  modifying database schemas, adding a table (e.g. Orders) to a DX.Data demo
  or app, writing FireDAC DataModules, defining entities with [Table]/[Column],
  using TSchemaReader / TSQLiteSchemaBuilder / TValidatorFactory / TResult<T>,
  TDataErrorHandler / TDataParams / IUnitOfWork / TDXMigrationRunner,
  handling EDataException errors, or structuring data access around DX.Data
  conventions.
---

# DX.Data

DX.Data is a FireDAC-based **micro-ORM** for Delphi: schema, validation,
mapping, transactions, migrations — without replacing FireDAC. No change
tracking, no lazy loading, no hidden SQL.

This skill documents the **current API** as implemented under `src/`. Older
drafts (`ctString`, `TValidationResult`, `TServiceFactory`, fluent
`TTableDef.Create.AddColumn` as the primary path) are obsolete, and so is the
old repository API: `TRepository`, `IQuery<T>`, `ICommand`,
`AssignParamValue`, `DX.Data.Repository.ErrorMapping`,
`TSchemaReader.ReadValidationRules<T>` and `EDataTypeMapException` no longer
exist (decision 0010).

## Core principles

1. **FireDAC stays visible.** Connections, queries, master-detail, and
   `TDataSource` are ordinary FireDAC. DX.Data fills the gaps: schema SSOT,
   validation, entity/JSON/dataset mapping.
2. **The entity is the schema.** Attributes on **private fields** (not
   properties) are reflected by `TSchemaReader`. One definition feeds DDL
   *and* validation.
3. **SQL stays visible.** Queries you write are the queries that run.
   Generated DDL (`BuildCreateTable`) is inspectable before `CreateTable`.
4. **Hybrid errors.** `TResult<T>` is for validation only. Database and
   infrastructure failures raise `EDataException` or one of its subclasses
   (`EDataConstraintException`, `EDataConnectionException`,
   `EDataLockException`, `EDataNotFoundException`, `EDataMigrationException`,
   `EDataMappingException`/`EDataSchemaException`). `TDataErrorHandler` is the
   only place that translates FireDAC errors (`Guard`, `HandleException`);
   `Capture` turns one *expected* error into a `TResult`. Programming errors
   are never translated. Details: `references/error-handling.md`.
5. **Scoped enums.** Always `TColumnType.&String`, `TColumnType.BigInt`,
   `TDefaultValue.AutoIncrement`. `&String` needs the ampersand (reserved
   word). `{$SCOPEDENUMS ON}` is already on in the schema units.

## Define an entity

Attributes sit on **private fields**. Optional columns use `TNullable<T>`,
never a bare type (a bare type cannot hold NULL and would write `''` / `0`
back over it).

```pascal
uses
  DX.Data.Schema.Attributes,
  DX.Data.Schema.Types,
  DX.Data.Nullable;

type
  [Table('CUSTOMERS')]
  TCustomer = class
  private
    [Column('ID', TColumnType.BigInt)]
    [PrimaryKey]
    [AutoIncrement]
    FID: Int64;

    [Column('COMPANY_NAME', TColumnType.&String, 40)]
    [Required]
    FCompanyName: string;

    [Column('CONTACT_NAME', TColumnType.&String, 30)]
    FContactName: TNullable<string>;

    [Column('EMAIL', TColumnType.&String, 60)]
    [Required]
    FEmail: string;

    [Column('DISCOUNT_PCT', TColumnType.Integer)]
    [Required]
    FDiscountPct: Integer;
  public
    property ID: Int64 read FID write FID;
    property CompanyName: string read FCompanyName write FCompanyName;
    property ContactName: TNullable<string> read FContactName write FContactName;
    property Email: string read FEmail write FEmail;
    property DiscountPct: Integer read FDiscountPct write FDiscountPct;
  end;
```

Worked examples in this repository: `demo/Demo.Customer.pas`,
`demo/Demo.Order.pas`, `demo/Demo.Product.pas` (convention-based entities).

### Numeric field types: the Delphi type decides

The field type decides whether a value is transported as floating point or
exactly — the same rule on every backend, whatever the column type:

| Delphi field type | Column type | Exact |
|---|---|---|
| `Double`, `Single`, `Extended` | `Float` | no |
| `Currency` | `Currency` (default 19,4) | yes, 4 decimal places |
| `TBcd` (`Data.FmtBcd`) | `Decimal` (default 18,4) | yes |

A `Double` field stays floating point even on a `Decimal` column. For money
and anything that is calculated with, use `Currency`; for more than four
decimal places use `TBcd` with explicit precision and scale (`TNullable<TBcd>`
is allowed):

```pascal
[Column('FACTOR', TColumnType.Decimal, 15, 6)]
FFactor: TBcd;
```

A `Currency` field on a column with more than four decimal places is rejected
when the schema is read. Backend limits: SQLite keeps `NUMERIC` as `REAL`
internally (15 significant decimal digits); on SQL Server a `Currency` column
without explicit precision/scale is `MONEY`, which FireDAC reads through a
floating-point field — give it an explicit precision and scale to get
`DECIMAL(p,s)`.

## Schema → DDL

There is **no** `TServiceFactory`. Construct the dialect builder with the
FireDAC connection and hold it as `ISchemaBuilder` (the builders are
reference-counted):

```pascal
uses
  DX.Data.Schema.Interfaces,
  DX.Data.Schema.Reader,
  DX.Data.Schema.Builder.SQLite;

begin
  var LBuilder: ISchemaBuilder := TSQLiteSchemaBuilder.Create(AConnection);
  // CREATE TABLE + CREATE INDEX — idempotent on every dialect, safe every start
  LBuilder.CreateTable(TSchemaReader.ReadTableDef<TCustomer>);
end;
```

Swap `TSQLiteSchemaBuilder` for `TPGSchemaBuilder` or `TMSSQLSchemaBuilder`.
The entity and `TSchemaReader` stay the same.

`CreateTable` executes `BuildCreateTableStatements` (one statement per list
entry). `BuildCreateTable` is a preview string for logging — never pass it to
`ExecSQL` and never split it. Primary keys keep their type; auto-increment
only on a single integer primary key (convention `FID`, or `[PrimaryKey]` +
`[AutoIncrement]`). Reserved words are quoted — use
`LBuilder.QuoteIdentifier` in hand-written SQL. `InsertSeedData`/
`ExecuteSchema` seed only empty tables, atomically; seeding in application
code (COUNT(*) then INSERT) is fine, too. Details:
`references/schema-builder.md`.

Fluent `TTableDef.Create('name').AddColumn(...)` still compiles, but the
supported path for applications is **attributes + `ReadTableDef<T>`**.

## Validation

```pascal
uses
  DX.Data.Validation.Interfaces,
  DX.Data.Validation.Factory,
  DX.Data.Validation.Rules,
  DX.Data.Result;

var
  LValidator: IValidator<TCustomer>;
  LResult: TResult<TCustomer>;
begin
  // Required + MaxLength are derived from the schema. Nothing else is.
  LValidator := TValidatorFactory.CreateFor<TCustomer>
    .AddRule(TRangeRule.Create('DISCOUNT_PCT', 0, 100))
    .AddRule(TRegexRule.Create('EMAIL', '^[^@\s]+@[^@\s]+\.[^@\s]+$',
      'E-mail address is not valid.'));

  LResult := LValidator.Validate(LCustomer);
  if LResult.IsFailure then
    for var LError in LResult.Errors do
      ShowError(LError.Field, LError.Message);   // TError: Code, Message, Field, Severity
end;
```

There is **no** `TValidationResult` / `IsValid` / `ErrorMessage`. Use
`TResult<T>` (`IsSuccess` / `IsFailure` / `Errors` / `ErrorMessages`).

`IValidator<T>.Rules` is `TArray<IFieldRule>`. Each rule has `FieldName`,
`Validate(const AValue: Variant): TResult<Boolean>`, and `Description`.
Filter by `FieldName` for `TField.OnValidate` (one cell, pre-write).

Built-in rules: `TRequiredRule`, `TMaxLengthRule`, `TRangeRule`,
`TRegexRule`, `TCustomRule`.

Rule field names are **column names** (`EMAIL`). `AddRule` with a name the
entity does not have raises `EArgumentException`; an invalid regex raises when
the rule is created. The convention primary key (auto-increment) gets no
required rule.

## Mapping

```pascal
uses
  System.JSON,
  FireDAC.Comp.Client,
  DX.Data.Schema.EntityMapper,
  DX.Data.Schema.JSONMapper,
  DX.Data.Schema.DataSetMapper;

begin
  // Row → entity (any TDataSet; MAPPER_NO_ROW only if closed or empty — checks IsEmpty, not Eof)
  var LCustomer := TEntityMapper.MapRow<TCustomer>(AQuery);

  // All rows; restores the position, frees created objects if a row fails
  var LCustomers := TEntityMapper.MapAll<TCustomer>(AQuery);

  // Entity → parameterized INSERT/UPDATE (missing params are skipped)
  TEntityMapper.MapToParams<TCustomer>(LCustomer, AQuery);

  // JSON
  var LJson := TJSONEntityMapper.MapAllToJSON<TCustomer>(LCustomers);
  LCustomers := TJSONEntityMapper.MapAllFromJSON<TCustomer>(LJson);

  // Dataset (MemTable field defs from the schema — no hard-coded column list)
  TDataSetMapper.MapAllToDataSet<TCustomer>(AMemTable, LCustomers);
end;
```

`TNullable<string>.Null` clears an optional field. Do not assign `nil`.

Mapper facts:

- Text binds as `ftWideString`/`ftWideMemo` (Unicode-safe). Do not add
  persistent `TStringField`s next to generated field definitions.
- Mapping errors raise `EDataMappingException` (`MAPPER_*`, `JSON_*`,
  `DECIMAL_*`); NULL in a `[Required]` column is `MAPPER_REQUIRED_NULL`.
  Their messages quote the rejected value.
- **PostgreSQL + GUID columns: the connection needs `GUIDEndian=Big`**
  (`Params.Values['GUIDEndian'] := 'Big'`), otherwise `MAPPER_GUID_ENDIAN`.
- Dates in SQLite text columns and JSON are ISO 8601; with a zone they are
  converted to local time, without one taken as local time.
- JSON is strictly typed (`JSON_TYPE`); `null` for a NOT NULL column raises
  `JSON_REQUIRED`; `TDate` → `YYYY-MM-DD`, `TTime` → `HH:MM:SS[.zzz]`.

## Data access and transactions

Data modules own `TFDQuery` components and wrap execution in
`TDataErrorHandler.Guard` (unit `DX.Data.ErrorHandler`). Assign
loose `Variant` values with `TDataParams.Assign` (unit
`DX.Data.Repository.Params`) — never `Param.Value := <string>` (binds
`ftString`, loses non-Latin-1 characters). Not found: raise
`EDataNotFoundException` (`DB_NOT_FOUND`) or return `nil`, as the method's
meaning requires.

```pascal
uses
  DX.Data.UnitOfWork.Interfaces,
  DX.Data.UnitOfWork;

begin
  var LUoW: IUnitOfWork := TUnitOfWork.Create(Connection);  // never a class reference
  FCustomers.Insert(LUoW.Connection, LCustomer);
  LUoW.Complete;  // commits now; without it the scope exit rolls back
end;
```

Migrations: `TDXMigrationRunner.MigrateUp` returns the number of applied
migrations and raises `EDataMigrationException` (`MIG_*`); call it at startup
without catching. SQL Server needs 2016+ for the unit of work. Codes and backend notes:
`references/unit-of-work.md`. Migrations: `references/migration.md`.

## Data-module pattern

A context data module owns the queries; a shared `TDMMain` owns the
`TFDConnection`. The form binds grids to `TDataSource` and displays
validation results. It does not contain SQL.

SQLite `TEXT` often arrives as `ftWideMemo`. Map it on the connection so
a `TDBGrid` shows values instead of `(WIDEMEMO)`:

```
FormatOptions.OwnMapRules = True
dtWideMemo → dtWideString
```

## Adding a table

Follow `references/adding-a-table.md` — a numbered walkthrough that adds
an `ORDERS` table to the trivial VCL demo (entity, schema bootstrap,
seed, master-detail grid, validator).

## Code generation checklist

- File encoding: **UTF-8 with BOM**; line endings: **CRLF**
- Unit header: XML doc `/// <summary>` + `<copyright>` (`Copyright © 2026 Olaf Monien`)
- Naming: `T` types, `F` fields, `L` locals, `A` parameters
- Table order: referenced tables before referencing tables
- Optional columns: `TNullable<T>`
- Attributes on fields, not properties
- `TColumnType.&String` (ampersand), never `ctString`

## References

- `references/adding-a-table.md` — guided Orders walkthrough for the trivial demo
- `references/schema-builder.md` — builder/DDL details (verify against source; prefer this SKILL.md for the entity API)
- `references/migration.md` — versioned migrations, `TDXMigrationRunner`, `TDXAlterTable`
- `references/error-handling.md` — exception hierarchy, codes, `TDataErrorHandler`, `TDataParams`
- `references/unit-of-work.md` — `IUnitOfWork` states, codes, backend notes
