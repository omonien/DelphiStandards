# DX.Data Schema Builder — Complete API Reference

Units: `DX.Data.Schema.Types` (definition types), `DX.Data.Schema.Interfaces` (`ISchemaBuilder`)

---

## TColumnType

```pascal
TColumnType = (
  Integer,   // 32-bit integer. PG: INTEGER, MSSQL: INT, SQLite: INTEGER
  BigInt,    // 64-bit integer. PG: BIGINT, MSSQL: BIGINT, SQLite: INTEGER
  &String,   // Variable-length string. PG: VARCHAR(n), MSSQL: NVARCHAR(n), SQLite: TEXT
  Text,      // Unlimited text. PG: TEXT, MSSQL: NVARCHAR(MAX), SQLite: TEXT
  Boolean,   // Boolean. PG: BOOLEAN, MSSQL: BIT, SQLite: INTEGER (0/1)
  DateTime,  // Date + time. PG: TIMESTAMPTZ, MSSQL: DATETIME2, SQLite: TEXT
  Date,      // Date only. PG: DATE, MSSQL: DATE, SQLite: TEXT
  Time,      // Time only. PG: TIME, MSSQL: TIME, SQLite: TEXT
  Float,     // Floating point (not exact). PG: DOUBLE PRECISION, MSSQL: FLOAT, SQLite: REAL
  Decimal,   // Exact decimal. PG: NUMERIC(p,s), MSSQL: DECIMAL(p,s), SQLite: NUMERIC(p,s)
  Currency,  // Exact decimal, default 19,4. PG: NUMERIC(p,s), MSSQL: MONEY (19,4) or DECIMAL(p,s), SQLite: NUMERIC(p,s)
  Blob,      // PG: BYTEA, MSSQL: VARBINARY(MAX), SQLite: BLOB
  Guid       // PG: UUID, MSSQL: UNIQUEIDENTIFIER, SQLite: TEXT
);
```

**Notes:**
- `TColumnType.&String` requires `Length > 0`. If `Length = 0`, PostgreSQL defaults to `VARCHAR(255)`.
- `TColumnType.Decimal` and `TColumnType.Currency` take `Precision` and `Scale` (set via the `TColumnDef.Decimal` factory or directly on the record). Without a value, `Decimal` is 18,4 and `Currency` is 19,4. Both are exact; only `Float` is floating point. In Delphi the entity field type decides the transport: `Double` = floating point, `Currency` and `TBcd` = exact (see decision 0009).
- SQLite stores `NUMERIC` columns internally as `REAL` and therefore carries 15 significant decimal digits.
- SQLite stores dates/timestamps as TEXT in ISO 8601 format.
- SQLite has no native boolean — use `0` and `1` as integer values.

---

## TDefaultValue

```pascal
TDefaultValue = (
  None,             // No DEFAULT clause generated
  AutoIncrement,    // only on a single-column integer PK (else SCHEMA_AUTOINC_TYPE / _NOT_PK)
  CurrentDate,      // local date of the server (SQLite date('now','localtime'), MSSQL CAST(SYSDATETIME() AS DATE))
  CurrentTime,      // local time of the server
  CurrentTimestamp, // PG CURRENT_TIMESTAMP, MSSQL SYSDATETIME(), SQLite UTC text with Z
  Zero,             // DEFAULT 0
  One,              // DEFAULT 1
  EmptyString,      // DEFAULT ''
  Custom            // Uses the DefaultExpr field — you must set it manually
);
```

---

## TForeignKeyAction

```pascal
TForeignKeyAction = (
  NoAction,   // ON DELETE/UPDATE: NO ACTION
  Cascade,    // ON DELETE/UPDATE: CASCADE
  SetNull,    // ON DELETE/UPDATE: SET NULL
  SetDefault, // ON DELETE/UPDATE: SET DEFAULT
  Restrict    // ON DELETE/UPDATE: RESTRICT (default of TForeignKeyDef.Create)
);
```

SQL Server has no `RESTRICT`; its builder emits `NO ACTION`, which checks
immediately and behaves the same.

---

## TColumnDef

```pascal
TColumnDef = record
  Name:        string;
  ColType:     TColumnType;
  Length:      Integer;      // For TColumnType.&String: VARCHAR(Length)
  Precision:   Integer;      // For Decimal/Currency: NUMERIC(Precision, Scale); 0 = type default
  Scale:       Integer;
  Nullable:    Boolean;      // False → NOT NULL
  DefaultVal:  TDefaultValue;
  DefaultExpr: string;       // Used when DefaultVal = TDefaultValue.Custom
  IsPrimaryKey: Boolean;
  IsUnique:    Boolean;      // Single-column UNIQUE constraint
  CheckExpr:   string;       // Optional CHECK constraint expression
end;
```

### Constructors

```pascal
// General column
class function TColumnDef.Create(
  const AName: string;        // Column name
  AType: TColumnType;         // Column type
  ALength: Integer = 0;       // For TColumnType.&String; 0 = use default
  ANullable: Boolean = True;  // True = NULL allowed
  ADefault: TDefaultValue = TDefaultValue.None
): TColumnDef;

// Single-column primary key, NOT NULL. Auto-increment only for Integer/BigInt;
// String or Guid keys keep their type and the application supplies the value.
class function TColumnDef.PrimaryKey(
  const AName: string;
  AType: TColumnType = TColumnType.BigInt
): TColumnDef;

// Exact decimal column (shorthand)
class function TColumnDef.Decimal(
  const AName: string;
  APrecision: Integer = 18;
  AScale: Integer = 4;
  ANullable: Boolean = False;
  ADefault: TDefaultValue = TDefaultValue.Zero
): TColumnDef;
```

### Setting CheckExpr and DefaultExpr

These fields are not part of the constructor signature — set them directly after creation:

```pascal
uses
  DX.Data.Schema.Types;

begin
  var LStatus := TColumnDef.Create('status', TColumnType.&String, 20, False, TDefaultValue.Custom);
  LStatus.DefaultExpr := '''New''';    // Note: escaped single quotes
  LStatus.CheckExpr   := 'status IN (''New'', ''Shipped'', ''Closed'')';
end;
```

When using the fluent chain on `TTableDef`, access the column by index:

```pascal
uses
  DX.Data.Schema.Types;

begin
  var LTable := TTableDef.Create('order')
    .AddColumn(TColumnDef.Create('country', TColumnType.&String, 100, True, TDefaultValue.Custom));
  // Column is at index 0 (if first column added):
  LTable.Columns[0].DefaultExpr := '''Germany''';
end;
```

---

## TForeignKeyDef

```pascal
TForeignKeyDef = record
  ColumnName: string;
  RefTable:   string;
  RefColumn:  string;
  OnDelete:   TForeignKeyAction;
  OnUpdate:   TForeignKeyAction;
end;

class function TForeignKeyDef.Create(
  const AColumn:   string;              // Column in this table
  const ARefTable: string;              // Referenced table name
  const ARefColumn: string;             // Referenced column name
  AOnDelete: TForeignKeyAction = TForeignKeyAction.Restrict;
  AOnUpdate: TForeignKeyAction = TForeignKeyAction.Restrict
): TForeignKeyDef;
```

**Examples:**

```pascal
// Simple FK (ON DELETE/UPDATE default: RESTRICT)
TForeignKeyDef.Create('customer_id', 'customer', 'customer_id')

// FK with CASCADE delete (orders deleted when customer is deleted)
TForeignKeyDef.Create('customer_id', 'customer', 'customer_id', TForeignKeyAction.Cascade)

// FK with SET NULL on delete
TForeignKeyDef.Create('assigned_to', 'user', 'user_id', TForeignKeyAction.SetNull)
```

---

## TIndexDef

```pascal
TIndexDef = record
  Name:     string;
  Columns:  TArray<string>;
  IsUnique: Boolean;
end;

class function TIndexDef.Create(
  const AName:    string;
  AColumns:       TArray<string>;
  AUnique:        Boolean = False
): TIndexDef;
```

**Examples:**

```pascal
// Single-column index
TIndexDef.Create('idx_customer_email', ['email'])

// Multi-column index
TIndexDef.Create('idx_order_customer_date', ['customer_id', 'order_date'])

// Unique index
TIndexDef.Create('uq_user_email', ['email'], True)
```

---

## TTableDef

```pascal
TTableDef = record
  Name:              string;
  Columns:           TArray<TColumnDef>;
  ForeignKeys:       TArray<TForeignKeyDef>;
  Indexes:           TArray<TIndexDef>;
  UniqueConstraints: TArray<TArray<string>>;
  CompositePK:       TArray<string>;   // PRIMARY KEY (col1, col2) clause
end;

class function TTableDef.Create(const AName: string): TTableDef;
function AddColumn(const ACol: TColumnDef): TTableDef;
function AddForeignKey(const AFK: TForeignKeyDef): TTableDef;
function AddIndex(const AIdx: TIndexDef): TTableDef;
function AddUnique(AColumns: TArray<string>): TTableDef;  // UNIQUE constraint
function SetCompositePK(AColumns: TArray<string>): TTableDef;
```

**Difference between `AddIndex` with `IsUnique=True` vs `AddUnique`:**
- `AddIndex(..., True)` → `CREATE UNIQUE INDEX` (index-based uniqueness, supports multi-column)
- `AddUnique(...)` → `UNIQUE (col1, col2)` inline constraint in `CREATE TABLE` (preferred for composite keys)

**Table order matters.** Return referenced tables before referencing tables in your `GetTables` function:

```pascal
class function TMySchema.GetTables: TArray<TTableDef>;
begin
  Result := [
    BuildCustomerTable,    // No FKs — comes first
    BuildOrderTable,       // FK to customer — comes second
    BuildOrderLineTable    // FK to order — comes third
  ];
end;
```

---

## TSeedDef / TSeedRow

```pascal
TSeedRow = TArray<TPair<string, string>>;  // Column name → string value

TSeedDef = record
  TableName: string;
  Rows:      TArray<TSeedRow>;
end;

class function TSeedDef.Create(const ATable: string): TSeedDef;
function AddRow(ARow: TSeedRow): TSeedDef;
```

**All values are strings** — the schema builder wraps them in single quotes and escapes embedded quotes (SQL Server: `N'…'`). An explicit `NULL` cannot be expressed; leave the column out instead.

```pascal
uses
  System.Generics.Collections,  // TPair
  DX.Data.Schema.Types;

begin
  var LSeed := TSeedDef.Create('product_category')
    .AddRow([
      TPair<string, string>.Create('name', 'Electronics'),
      TPair<string, string>.Create('code', 'ELEC')
    ])
    .AddRow([
      TPair<string, string>.Create('name', 'O''Reilly Books'),  // Escaped: O'Reilly
      TPair<string, string>.Create('code', 'BOOK')
    ]);
end;
```

Seed data is only inserted when the target table is **empty** (`COUNT(*) = 0`), atomically — in its own transaction, or in a savepoint when the caller holds one (migration, unit of work). It is safe to call `ExecuteSchema` on every startup. Rows with an explicit ID work on every backend: SQL Server switches `IDENTITY_INSERT` per row, PostgreSQL uses `OVERRIDING SYSTEM VALUE` and moves the identity behind the largest ID.

---

## ISchemaBuilder

```pascal
ISchemaBuilder = interface
  ['{E1F2A3B4-C5D6-7890-ABCD-EF0123456789}']
  // DDL as text, nothing executed
  function BuildCreateTable(const ATableDef: TTableDef): string;            // preview only
  function BuildCreateTableStatements(const ATableDef: TTableDef): TArray<string>;
  function BuildDropTable(const ATableName: string): string;
  function BuildAddColumn(const ATableName: string; const ACol: TColumnDef): string;
  function BuildDropColumn(const ATableName, AColumnName: string): string; overload;
  function BuildDropColumn(const ATableDef: TTableDef;
    const AColumnName: string): TArray<string>; overload;                    // SQLite: rebuild
  function BuildCreateIndex(const ATableName: string; const AIdx: TIndexDef): string;
  function BuildDropIndex(const ATableName, AIndexName: string): string;
  function QuoteIdentifier(const AName: string): string;
  // Execution
  procedure CreateTable(const ATableDef: TTableDef);
  procedure DropColumn(const ATableDef: TTableDef; const AColumnName: string);
  procedure CreateIndex(const ATableName: string; const AIndexDef: TIndexDef);
  procedure InsertSeedData(const ASeedDef: TSeedDef);
  procedure ExecuteSchema(const ATables: TArray<TTableDef>;
    const ASeeds: TArray<TSeedDef>);
end;
```

- `BuildCreateTable` joins the statements with `;` and a line break for logging. Execute with `CreateTable` or loop over `BuildCreateTableStatements` — never split the preview.
- `CreateTable` and `CreateIndex` are idempotent on every dialect (`IF NOT EXISTS`; SQL Server: existence check first).
- `BuildDropIndex` needs the table because SQL Server finds an index only via `ON <table>`.
- **Quoting:** reserved words and names with characters other than letters, digits and `_` are quoted (`"…"`, SQL Server `[…]`); normal names stay unquoted. A dot outside quotes separates qualifiers. On PostgreSQL a quoted `"ORDER"` is case-sensitive — quote it the same way in hand-written SQL via `QuoteIdentifier`. CHECK and DEFAULT expressions are not touched.
- **Primary keys** keep their type; identity only with `TDefaultValue.AutoIncrement` on a single integer PK. On SQLite an `INTEGER PRIMARY KEY` is a rowid alias and gets an ID even without auto-increment.
- **SQLite `DropColumn(TTableDef, Column)`** rebuilds the table as documented by SQLite and aborts before changing anything on `SCHEMA_SQLITE_REBUILD_DEF_MISMATCH` (TTableDef ≠ real table), `SCHEMA_SQLITE_REBUILD_DEPENDENTS` (triggers/views) or `SCHEMA_SQLITE_REBUILD_IN_TRANSACTION` (transaction running and another table references this one). New FK violations: `SCHEMA_SQLITE_FK_CHECK`, rolled back.
- **SQL Server `DropColumn(TTableDef, Column)`** removes default, CHECK, FK and UNIQUE constraints and indexes of the column first; the plain `BuildDropColumn(Table, Column)` does not.
- All schema errors raise `EDataSchemaException` (`SCHEMA_*`).
- The executing methods (`CreateTable`, `CreateIndex`, `DropColumn`, `InsertSeedData`, `ExecuteSchema`) translate database errors through `TDataErrorHandler.Guard` after their own rollback (e.g. a unique violation in a seed → `EDataConstraintException`). The builders use `DX.Data.ErrorHandler` (folder `src/DX.Data.Result`).

### Implementations

| Class | Unit | Schema Prefix |
|---|---|---|
| `TSQLiteSchemaBuilder` | `DX.Data.Schema.Builder.SQLite` | Always `''` (SQLite has no schemas) |
| `TPGSchemaBuilder` | `DX.Data.Schema.Builder.PG` | e.g. `'myapp.'` — creates schema with `CREATE SCHEMA IF NOT EXISTS` |
| `TMSSQLSchemaBuilder` | `DX.Data.Schema.Builder.MSSQL` | e.g. `'myapp.'` — creates schema with `EXEC sp_executesql` |

### Constructor

```pascal
// All three builders have this constructor (via TSchemaBuilderBase):
constructor Create(
  const AConnection: TFDConnection;
  const ASchemaPrefix: string = ''
);
```

### TSchemaBuilderBase (abstract base)

Override these in subclasses:

```pascal
function MapColumnType(const ACol: TColumnDef): string; virtual; abstract;
function MapDefault(const ACol: TColumnDef): string; virtual; abstract;
function MapAutoIncrementPrimaryKey(const ACol: TColumnDef): string; virtual; abstract;
function IsReservedWord(const AName: string): Boolean; virtual; abstract;
function MapPrimaryKey(const ACol: TColumnDef): string; virtual;  // no longer abstract
procedure BeforeSchema; virtual;  // called by ExecuteSchema before the tables
```

`BeforeSchema` (only from `ExecuteSchema`, not from `CreateTable`) uses:
- **SQLite:** `PRAGMA foreign_keys = ON`
- **PostgreSQL:** `CREATE SCHEMA IF NOT EXISTS <name>` (if schema prefix is set)
- **MSSQL:** Creates schema via `EXEC sp_executesql` (if schema prefix is set)

---

## Complete Schema Module Example

```pascal
unit Order.Schema;

interface

uses
  System.Generics.Collections,  // TPair in GetSeedData
  DX.Data.Schema.Types;

type
  TOrderSchema = class
    class function GetTables: TArray<TTableDef>; static;
    class function GetSeedData: TArray<TSeedDef>; static;
  end;

implementation

class function TOrderSchema.GetTables: TArray<TTableDef>;
var
  LCategory, LProduct, LOrder, LOrderLine: TTableDef;
  LStatusCol: TColumnDef;
begin
  LCategory := TTableDef.Create('category')
    .AddColumn(TColumnDef.PrimaryKey('category_id'))
    .AddColumn(TColumnDef.Create('name', TColumnType.&String, 100, False))
    .AddColumn(TColumnDef.Create('code', TColumnType.&String, 20,  False));
  LCategory := LCategory.AddUnique(['code']);  // AddUnique returns a new value — reassign!

  LProduct := TTableDef.Create('product')
    .AddColumn(TColumnDef.PrimaryKey('product_id'))
    .AddColumn(TColumnDef.Create('category_id', TColumnType.BigInt,  0,   False))
    .AddColumn(TColumnDef.Create('name',         TColumnType.&String,  200, False))
    .AddColumn(TColumnDef.Create('sku',          TColumnType.&String,  50,  False))
    .AddColumn(TColumnDef.Decimal('price', 18, 4, False, TDefaultValue.Zero))
    .AddForeignKey(TForeignKeyDef.Create('category_id', 'category', 'category_id'))
    .AddIndex(TIndexDef.Create('idx_product_sku', ['sku'], True));

  LStatusCol := TColumnDef.Create('status', TColumnType.&String, 20, False, TDefaultValue.Custom);
  LStatusCol.DefaultExpr := '''New''';
  LStatusCol.CheckExpr   := 'status IN (''New'', ''Confirmed'', ''Shipped'', ''Cancelled'')';

  LOrder := TTableDef.Create('sales_order')
    .AddColumn(TColumnDef.PrimaryKey('order_id'))
    .AddColumn(TColumnDef.Create('order_date', TColumnType.Date, 0, False, TDefaultValue.CurrentDate))
    .AddColumn(LStatusCol)
    .AddColumn(TColumnDef.Decimal('total', 18, 4, False, TDefaultValue.Zero))
    .AddColumn(TColumnDef.Create('created_at', TColumnType.DateTime, 0, False, TDefaultValue.CurrentTimestamp))
    .AddIndex(TIndexDef.Create('idx_order_date', ['order_date']));

  LOrderLine := TTableDef.Create('order_line')
    .AddColumn(TColumnDef.PrimaryKey('line_id'))
    .AddColumn(TColumnDef.Create('order_id',   TColumnType.BigInt,  0, False))
    .AddColumn(TColumnDef.Create('product_id', TColumnType.BigInt,  0, False))
    .AddColumn(TColumnDef.Create('qty',         TColumnType.Integer, 0, False))
    .AddColumn(TColumnDef.Decimal('unit_price', 18, 4, False, TDefaultValue.Zero))
    .AddColumn(TColumnDef.Decimal('line_total', 18, 4, False, TDefaultValue.Zero))
    .AddForeignKey(TForeignKeyDef.Create('order_id',   'sales_order', 'order_id',   TForeignKeyAction.Cascade))
    .AddForeignKey(TForeignKeyDef.Create('product_id', 'product',     'product_id'))
    .AddUnique(['order_id', 'product_id'])
    .AddIndex(TIndexDef.Create('idx_line_order', ['order_id']));

  Result := [LCategory, LProduct, LOrder, LOrderLine];
end;

class function TOrderSchema.GetSeedData: TArray<TSeedDef>;
begin
  Result := [
    TSeedDef.Create('category')
      .AddRow([TPair<string,string>.Create('name', 'General'),
               TPair<string,string>.Create('code', 'GEN')])
  ];
end;

end.
```

---

## AddUnique — Important Note

`AddUnique` (like all fluent methods on `TTableDef`) returns a **new copy** of the record. Always reassign:

```pascal
// CORRECT
LTable := LTable.AddUnique(['col1', 'col2']);

// Also correct (inline in chain)
LTable := TTableDef.Create('foo')
  .AddColumn(...)
  .AddUnique(['col1', 'col2']);

// WRONG — result is discarded
LTable.AddUnique(['col1', 'col2']);
```
