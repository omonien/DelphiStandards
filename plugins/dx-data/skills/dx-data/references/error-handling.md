# DX.Data Error Handling — Reference

Units: `DX.Data.Result` (`TError`, `TResult<T>`), `DX.Data.Exceptions`
(hierarchy, code constants), `DX.Data.ErrorHandler` (folder `src/DX.Data.Result`)
(`TDataErrorHandler`), `DX.Data.Repository.Params` (`TDataParams`).
Background: decision record `.planning/decisions/0010-fehlerkonzept.md`.

## The rule

- **Validation** returns `TResult<T>` — collect all errors, show them.
- **Database and infrastructure** raise the `EDataException` hierarchy.
- **Programming errors** are never translated. Argument errors without a
  database context (`nil`, invalid value, non-matching definition) raise
  `EArgumentNilException`/`EArgumentException`/`EArgumentOutOfRangeException`.
  State errors of library objects with their own code (`UOW_NOT_ACTIVE`,
  `MIG_ALTER_APPLIED`, …) are `EDataException` or one of its subclasses.
- `IsPassThroughException` (`DX.Data.Exceptions`) is the one definition of what
  is never wrapped: `EExternal` (access violation etc.), `EOutOfMemory`,
  `EInvalidPointer`, `EAbort`.

Do not wrap database calls in code that turns exceptions into `TResult`.
Do not write `on E: Exception do Result := TResult.Failure(...)`.

## Hierarchy

| Class | For |
|---|---|
| `EDataException` | base; UoW state errors; `DB_CANCELLED` |
| `EDataValidationException` | forced failed `TResult` (`Value`, `ValueOrRaise`, `EnsureSuccess`) |
| `EDataConstraintException` | `Kind: TConstraintKind` = Unique, ForeignKey, NotNull, Check, Length, Range, Unknown |
| `EDataConnectionException` | `DB_CONNECTION_ERROR`, `DB_TIMEOUT`, `UOW_TX_LOST`, `REST_UNREACHABLE`, `REST_SERVER_ERROR` |
| `EDataLockException` | `DB_LOCK_TIMEOUT`, `DB_DEADLOCK`, `DB_SERIALIZATION_FAILURE` — retry the whole transaction |
| `EDataNotFoundException` | `DB_NOT_FOUND`, `DB_MULTIPLE_ROWS` (raised by your data module), `REST_NOT_FOUND` |
| `EDataMigrationException` | all `MIG_*` codes (constants in `DX.Data.Migration.Interfaces`); `Version`, `Direction` |
| `EDataMappingException` | `MAPPER_*`, `DSMAPPER_APPLY`, `JSON_*`, `DECIMAL_*`, `PARAM_*`, `REST_NO_PK`, `REST_COMPOSITE_PK` |
| `EDataSchemaException` | subclass of mapping: all `SCHEMA_*` codes |
| `ERestException` | `REST_CLIENT_ERROR`, `REST_BAD_JSON`, `REST_UNEXPECTED_STATUS`, `REST_NO_CONTENT` |

Members of `EDataException`: `Errors: TArray<TError>`, `FirstError`, `Code`
(first code, `''` if none), `HasErrorCode`, `HasAnyErrorCode`, `DBInfo`
(`NativeCode`, `SQLState`, `ConstraintName`, `TableName`, `ColumnName`,
`SQL`, `Detail`). Use the constants (`cErrDBUniqueViolation`, …), not
literals.

## TDataErrorHandler

```pascal
TDataErrorHandler = class sealed
  class function Translate(const AException: Exception; const ASQL: string = ''): EDataException;
  class procedure HandleException(const AException: Exception; const ASQL: string = ''); noreturn;
  class procedure Guard(const AProc: TProc; const ASQL: string = ''); overload;
  class function Guard<T>(const AFunc: TFunc<T>; const ASQL: string = ''): T; overload;
  class function Capture<T>(const AFunc: TFunc<T>; AClass: EDataExceptionClass;
    const ACodes: array of string): TResult<T>; overload;
  class function Capture<T>(AConnection: TFDConnection; const AFunc: TFunc<T>;
    AClass: EDataExceptionClass; const ACodes: array of string): TResult<T>; overload;
  class function CaptureStatus(const AProc: TProc; AClass: EDataExceptionClass;
    const ACodes: array of string): TResultStatus; overload;
  class function CaptureStatus(AConnection: TFDConnection; const AProc: TProc;
    AClass: EDataExceptionClass; const ACodes: array of string): TResultStatus; overload;
end;

TDataTransactionLevels = class sealed
  // nil on success, otherwise the rollback error as an unraised object (caller frees)
  class function RollbackTopLevel(AConnection: TFDCustomConnection): Exception; overload;
  class function RollbackAllLevels(AConnection: TFDCustomConnection): Exception; overload;
  class function NestingLevel(AConnection: TFDCustomConnection): LongWord; overload;
  // overloads for TFDCustomTransaction exist, too
end;
```

`TDataTransactionLevels` closes a savepoint level the server already dropped
(SQL Server `XACT_ABORT ON`: 6401/3903; PostgreSQL 3B001) without rollback —
never the outermost level. Use it instead of looping over `Rollback` yourself.
`RollbackAllLevels` returns the first error even if the transaction ended up
closed (check `NestingLevel`/`InTransaction`); `RollbackTopLevel` re-raises
`IsPassThroughException` classes unchanged.

- Translates only FireDAC database errors (`EFDDBEngineException`, plus
  `EFDException` with `FDCode` 700 = FireDAC timeout). Everything else passes
  through as the same object. The original is the `InnerException`.
- Classifies by native code (SQLite extended result code, PostgreSQL SQLSTATE,
  SQL Server error number; 2627/2601/1505 → `DB_UNIQUE_VIOLATION`); FireDAC
  `Kind` only as fallback. SQL Server 547 (FK vs. CHECK) is decided by the
  keyword before the first quote.
- Schema builders (executing methods), `TDXAlterTable.Apply`, the unit of work
  and the migration runner translate through it themselves. Because the unit
  lives in `src/DX.Data.Result`, projects using only schema, unit-of-work or
  migration units do not need `src/DX.Data.Repository` on the search path.
- `TError.Message` contains no data values; PostgreSQL DETAIL and SQL Server
  native messages for 2627/2601/1505/2628/8152/220/232 go to `DBInfo.Detail`.
  Mapper messages *do* quote the rejected value.
- PostgreSQL `DB_TIMEOUT` includes cancellation (57014 for both); SQL Server
  reports cancellation as `DB_CANCELLED`.

Data module pattern:

```pascal
uses
  System.SysUtils,
  DX.Data.ErrorHandler,
  DX.Data.Schema.EntityMapper;

procedure TDMCustomer.Insert(const ACustomer: TCustomer);
begin
  TDataErrorHandler.Guard(
    procedure
    begin
      TEntityMapper.MapToParams<TCustomer>(ACustomer, QryInsert);
      QryInsert.ExecSQL;
    end, QryInsert.SQL.Text);
end;

procedure TDMCustomer.Delete(AID: Int64);
begin
  try
    QryDelete.ParamByName('ID').AsLargeInt := AID;
    QryDelete.ExecSQL;
  except
    on E: Exception do
      TDataErrorHandler.HandleException(E, QryDelete.SQL.Text);  // never returns
  end;
end;
```

Call `HandleException` only inside the `except` block, with the exception
being handled. Never `raise E` on a caught object (double free).

Expected database error → `TResult` (the only sanctioned bridge):

```pascal
uses
  DX.Data.Result,
  DX.Data.Exceptions,
  DX.Data.ErrorHandler;

begin
  var LResult := TDataErrorHandler.CaptureStatus(Connection,
    procedure
    begin
      FCustomers.Insert(LCustomer);
    end,
    EDataConstraintException, [cErrDBUniqueViolation]);
  if LResult.IsFailure then
    ShowFieldError('EMAIL', 'This e-mail address is already taken.');
end;
```

Pass the connection whenever a transaction may be running: `Capture` then
sets a savepoint and rolls back only to it (keeps PostgreSQL transactions
usable). If that savepoint cannot be rolled back (SQL Server
`XACT_ABORT ON`, lost connection), the error keeps flying. Without a
connection no savepoint is set. The failure result loses `DBInfo`.

## TDataParams

```pascal
uses
  Data.DB,                    // ftInteger
  DX.Data.Repository.Params;

begin
  TDataParams.Assign(AParam, AValue);               // NULL only if the param already has a type
  TDataParams.Assign(AParam, AValue, ftInteger);    // type used if AValue is NULL
  TDataParams.AssignNull(AParam, ftInteger);        // ftUnknown → EArgumentException
  TDataParams.AssignGuid(AParam, LGuid);            // ftGuid; PostgreSQL needs GUIDEndian=Big
end;
```

Strings bind as `ftWideString` (assigning a string to `TFDParam.Value`
directly binds `ftString` and turns non-Latin-1 characters into `?`).
`Currency` → `ftBCD` 19,4, FMTBcd → `ftFMTBcd`, `Double` → `ftFloat`.
`PARAM_NULL_TYPE` for NULL without a type, `PARAM_TYPE` for unsupported
variants (arrays, interfaces).
