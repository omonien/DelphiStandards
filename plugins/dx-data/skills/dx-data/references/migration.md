# DX.Data Migrations — API Reference

Units: `DX.Data.Migration.Interfaces` (`IMigration`, `IMigrationRunner`,
`INonTransactionalMigration`, `IMigrationBackup`, all `cMigError…` codes),
`DX.Data.Migration.Base` (`TDXMigration`), `DX.Data.Migration.Runner`
(`TDXMigrationRunner`, `cVersionTableName`), `DX.Data.Migration.AlterTable`
(`TDXAlterTable`, `IDXAlterTable`), `DX.Data.Migration.Backup.SQLite`
(`TSQLiteMigrationBackup`).

## When to use what

| Scenario | Use |
|---|---|
| Fresh database, latest schema | `ISchemaBuilder.ExecuteSchema` / `CreateTable` (idempotent) |
| Change an already deployed schema | a migration (`TDXMigration`) run by `TDXMigrationRunner` |
| Add/drop columns or indexes inside a migration | `TDXAlterTable.ForTable(...)` |

Never modify a migration that has been applied anywhere; add a new one.

## IMigration and TDXMigration

```pascal
IMigration = interface
  function Version: Integer;      // contiguous from 1, no gaps, no duplicates
  function Description: string;
  procedure Up(const AConnection: TFDConnection; const ABuilder: ISchemaBuilder);
  procedure Down(const AConnection: TFDConnection; const ABuilder: ISchemaBuilder);
end;

TDXMigration = class abstract(TInterfacedObject, IMigration, INonTransactionalMigration)
protected
  procedure Execute(const AConnection: TFDConnection; const ASQL: string);
public
  function Version: Integer; virtual; abstract;
  function Description: string; virtual; abstract;
  procedure Up(...); virtual; abstract;
  procedure Down(...); virtual; abstract;
  function RunWithoutTransaction: Boolean; virtual;  // default False
end;
```

Generate DDL through `ABuilder` so the migration is valid on every backend.
`BuildCreateTable` returns a preview string only — execute with
`ABuilder.CreateTable`, or run `BuildCreateTableStatements` one by one.

```pascal
uses
  FireDAC.Comp.Client,
  DX.Data.Schema.Types,
  DX.Data.Schema.Interfaces,
  DX.Data.Schema.Reader,
  DX.Data.Migration.Base,
  DX.Data.Migration.AlterTable;

type
  TMigration001CreateCustomers = class(TDXMigration)
  public
    function Version: Integer; override;
    function Description: string; override;
    procedure Up(const AConnection: TFDConnection; const ABuilder: ISchemaBuilder); override;
    procedure Down(const AConnection: TFDConnection; const ABuilder: ISchemaBuilder); override;
  end;

  TMigration002AddPhone = class(TDXMigration)
  public
    function Version: Integer; override;
    function Description: string; override;
    procedure Up(const AConnection: TFDConnection; const ABuilder: ISchemaBuilder); override;
    procedure Down(const AConnection: TFDConnection; const ABuilder: ISchemaBuilder); override;
  end;

function TMigration001CreateCustomers.Version: Integer;
begin
  Result := 1;
end;

function TMigration001CreateCustomers.Description: string;
begin
  Result := 'Create CUSTOMERS';
end;

procedure TMigration001CreateCustomers.Up(const AConnection: TFDConnection;
  const ABuilder: ISchemaBuilder);
begin
  ABuilder.CreateTable(TSchemaReader.ReadTableDef<TCustomer>);
end;

procedure TMigration001CreateCustomers.Down(const AConnection: TFDConnection;
  const ABuilder: ISchemaBuilder);
begin
  Execute(AConnection, ABuilder.BuildDropTable('CUSTOMERS'));
end;

function TMigration002AddPhone.Version: Integer;
begin
  Result := 2;
end;

function TMigration002AddPhone.Description: string;
begin
  Result := 'Add CUSTOMERS.PHONE';
end;

procedure TMigration002AddPhone.Up(const AConnection: TFDConnection;
  const ABuilder: ISchemaBuilder);
begin
  TDXAlterTable.ForTable('CUSTOMERS', AConnection, ABuilder)
    .AddColumn(TColumnDef.Create('PHONE', TColumnType.&String, 50))
    .AddIndex(TIndexDef.Create('IX_CUSTOMERS_PHONE', ['PHONE']))
    .Apply;
end;

procedure TMigration002AddPhone.Down(const AConnection: TFDConnection;
  const ABuilder: ISchemaBuilder);
begin
  TDXAlterTable.ForTable('CUSTOMERS', AConnection, ABuilder)
    .DropIndex('IX_CUSTOMERS_PHONE')
    .DropColumn('PHONE')
    .Apply;
end;
```

A migration must not call `AConnection.Commit`/`Rollback` on the runner's
transaction (`MIG_TRANSACTION_ENDED`).

## TDXAlterTable

```pascal
class function TDXAlterTable.ForTable(const ATableName: string;
  const AConnection: TFDConnection; const ABuilder: ISchemaBuilder): IDXAlterTable;

IDXAlterTable = interface
  function AddColumn(const ACol: TColumnDef): IDXAlterTable;
  function DropColumn(const AColumnName: string): IDXAlterTable; overload;
  function DropColumn(const ATableDef: TTableDef; const AColumnName: string): IDXAlterTable; overload;
  function AddIndex(const AIdx: TIndexDef): IDXAlterTable;
  function DropIndex(const AIndexName: string): IDXAlterTable;
  procedure Apply;
  function Preview: TArray<string>;
end;
```

- The chain is an interface and frees itself — no `try/finally Free`. The
  constructor is protected; always go through `ForTable`.
- `Apply` runs once. A second `Apply` or another step afterwards raises
  `EDataMigrationException` `MIG_ALTER_APPLIED`, also after a failure.
- Database errors in `Apply` are translated by `TDataErrorHandler` after the
  rollback (FireDAC exception as `InnerException`); a savepoint the server
  already dropped is closed via `TDataTransactionLevels.RollbackTopLevel`.
- Consecutive simple steps run together atomically (own transaction, or a
  savepoint inside the runner's transaction).
- Plain `DropColumn(Name)` fails for columns with a key, default, UNIQUE,
  CHECK or index (SQLite and SQL Server). Use `DropColumn(TableDef, Name)`:
  SQL Server removes the dependent constraints and indexes first, SQLite
  rebuilds the table. A `TTableDef` for a different table raises
  `EArgumentException`.

## SQLite table rebuild inside migrations

`DropColumn(TableDef, Name)` on SQLite rebuilds the table (foreign keys off,
copy, drop, rename, indexes, `PRAGMA foreign_key_check`, foreign keys on).
It aborts before changing anything with `EDataSchemaException` when

- the `TTableDef` does not match the real table
  (`SCHEMA_SQLITE_REBUILD_DEF_MISMATCH`),
- triggers or views depend on the table (`SCHEMA_SQLITE_REBUILD_DEPENDENTS`),
- a transaction is running — so foreign keys cannot be switched off — and
  another table references this one (`SCHEMA_SQLITE_REBUILD_IN_TRANSACTION`).

Because the runner holds every migration in a transaction, the last case
means: dropping a column from a referenced table on SQLite needs a migration
without a transaction (see below). With a transaction it fails safely; the
data stays unchanged.

## TDXMigrationRunner

```pascal
constructor TDXMigrationRunner.Create(const AConnection: TFDConnection;
  const ASchemaBuilder: ISchemaBuilder;
  const AVersionTableName: string = cVersionTableName;  // 'dx_schema_versions'
  const ABackup: IMigrationBackup = nil);

IMigrationRunner = interface
  ['{6D934D0E-7F30-434B-964A-59895139B561}']
  procedure RegisterMigration(const AMigration: IMigration);
  function MigrateUp: Integer;                            // raises EDataMigrationException
  function MigrateDown(ATargetVersion: Integer): Integer; // raises EDataMigrationException
  procedure Validate;                                     // checks of MigrateUp, writes nothing
  function CurrentVersion: Integer;
  function PendingMigrations: TArray<IMigration>;
  function AppliedMigrations: TArray<Integer>;
  function DryRun: TArray<string>;
end;
```

Startup pattern:

```pascal
uses
  FireDAC.Comp.Client,
  DX.Data.Schema.Interfaces,
  DX.Data.Schema.Builder.SQLite,
  DX.Data.Migration.Interfaces,
  DX.Data.Migration.Runner;

procedure MigrateDatabase(AConnection: TFDConnection);
begin
  var LBuilder: ISchemaBuilder := TSQLiteSchemaBuilder.Create(AConnection);
  var LRunner: IMigrationRunner := TDXMigrationRunner.Create(AConnection, LBuilder);
  LRunner.RegisterMigration(TMigration001CreateCustomers.Create);
  LRunner.RegisterMigration(TMigration002AddPhone.Create);
  // Raises EDataMigrationException; let it end the start — never run the
  // application on a half-migrated schema.
  LRunner.MigrateUp;
end;
```

Behaviour:

- **One transaction per migration**: DDL and version entry commit or roll
  back together. The connection must not already be in a transaction
  (`MIG_IN_TRANSACTION`).
- **Checks before anything runs** (both directions), in this order:
  `MIG_IN_TRANSACTION`, `MIG_DUPLICATE`/`MIG_GAP` (registered versions not
  contiguous from 1), `MIG_AUTOCOMMIT_OFF`, then `MIG_VERSION_TABLE` and
  `MIG_AHEAD`/`MIG_APPLIED_GAP` (Up) or `MIG_NOT_FOUND` (Down). `Validate`
  runs the Up checks without writing (never `MIG_VERSION_TABLE`).
- **All codes are `EDataMigrationException`** with `Version`/`Direction` of the
  step (0 and the run's direction for a rejected run). Use the `cMigError…`
  constants.
- **Version table** (`version` Integer NOT NULL UNIQUE, `description` Text,
  `applied_at` DateTime default `CurrentTimestamp`) is created through the
  builder on the first `MigrateUp`. It is addressed unqualified: it must live
  in the default schema (SQL Server) or the `search_path` (PostgreSQL); a
  builder with a schema prefix leads to `MIG_VERSION_TABLE`. The name must be
  a simple identifier and not a reserved word.
- **Read methods never write**: `CurrentVersion`, `PendingMigrations`,
  `AppliedMigrations`, `Validate`, `DryRun` do not create the version table.
  `DryRun` returns one comment line per pending migration (suffix
  ` (ohne Transaktion)` for migrations without a transaction), not DDL.
  Database errors in read methods arrive translated (the `EDataException`
  hierarchy).
- **`MigrateDown(N)`** undoes versions greater than `N`; `N` stays applied,
  `0` undoes all, negative raises `EArgumentOutOfRangeException`.
- Failures: `MIG_FAILED` (Up failed, rolled back; earlier ones stay),
  `MIG_DOWN_FAILED` (Down failed, rolled back; version stays). Only
  `EFDException`/`EDataException` are wrapped: `Errors` holds the MIG code
  first, then the cause's (translated) errors, so
  `E.HasErrorCode(cErrDBUniqueViolation)` works; `DBInfo` from the
  translation; original as `InnerException`. If the transaction was already
  closed when the step failed, nothing was rolled back: with an intact
  connection and a non-connection cause, `MIG_FAILED` also carries
  `MIG_TRANSACTION_ENDED`; after a lost connection the message is neutral and
  `DB_CONNECTION_ERROR` follows `MIG_FAILED`.
- **Programming errors** in `Up`/`Down` (access violation, `EAbort`, own
  classes) arrive unchanged after the rollback. Database errors outside a step
  arrive translated as from `TDataErrorHandler.Guard`.
- A migration that leaves its own transaction (or inner level) open:
  `MIG_TRANSACTION_LEFT_OPEN`, rolled back.
- Logged through `IDataLogger` per migration (start, success, failure,
  rollback) and per rejected run.
- **PostgreSQL with `TxOptions.AutoCommit = False`**: FireDAC opens a
  transaction even for reads. A read-only `MigrateDown` leaves it open and the
  next runner call on that connection gets `MIG_IN_TRANSACTION`. End the
  transaction first.

## Migrations without a transaction

Override `TDXMigration.RunWithoutTransaction` to return `True` (or implement
`INonTransactionalMigration`) only for steps that cannot run in a
transaction: the SQLite rebuild of a referenced table, or PostgreSQL
`CREATE INDEX CONCURRENTLY`.

- Not atomic: on failure earlier changes remain, the version is not recorded
  (`MIG_FAILED`/`MIG_DOWN_FAILED` say so). Keep such migrations small and
  idempotent.
- The runner records the version afterwards in its own short transaction.
  If only that fails: `MIG_VERSION_NOT_RECORDED` with the `INSERT`/`DELETE`
  that fixes it by hand; without the fix the next run repeats the step.
- Requires `TxOptions.AutoCommit = True` whenever such a migration could run
  (`MigrateUp`: any registered one, even applied; `MigrateDown`: any above the
  target) — otherwise `MIG_AUTOCOMMIT_OFF` before the database is touched.
- `MIG_VERSION_RECORDED_ELSEWHERE`: the version was already recorded after the
  step (parallel runner, or own commit succeeded despite an error). Parallel
  runners are not protected.
- `MIG_TRANSACTION_LEFT_OPEN`: the migration left its own transaction open;
  it is rolled back.
- `RunWithoutTransaction` is evaluated once per run for every registered
  migration: it must not raise and must not depend on database state.

**Optional backup.** Pass an `IMigrationBackup` to the runner's constructor;
it runs right before each migration without a transaction, in both
directions. `TSQLiteMigrationBackup.Create(ATargetDirectory = '')` (unit
`DX.Data.Migration.Backup.SQLite`) writes a copy with `VACUUM INTO`
(SQLite 3.27+) next to the database or into an existing directory. Other backends, in-memory and encrypted databases and
missing directories are rejected with `MIG_BACKUP_FAILED`, and the migration
does not run. Any exception from a backup becomes `MIG_BACKUP_FAILED`, except
`IsPassThroughException` ones (`EExternal`, `EOutOfMemory`,
`EInvalidPointer`, `EAbort`), which arrive unchanged. Backups hold all data, are never deleted automatically, capture
the state *before* the migration and do not replace a transaction.

## Rules for writing migrations

1. Never modify an applied migration; add a new one.
2. Implement and test both `Up` and `Down`.
3. Use the builder (`CreateTable`, `TDXAlterTable`, `Build…`) instead of
   dialect SQL; quote hand-written names with `ABuilder.QuoteIdentifier`.
4. Keep migrations small — one logical change each.
5. On SQLite, test column drops on referenced tables explicitly.
