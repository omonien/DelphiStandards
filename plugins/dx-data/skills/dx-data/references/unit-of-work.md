# DX.Data Unit of Work — Reference

Units: `DX.Data.UnitOfWork.Interfaces` (`IUnitOfWork`, `IUnitOfWorkFactory`,
`TUnitOfWorkState`, `cErrUow*` constants), `DX.Data.UnitOfWork`
(`TUnitOfWork`, `TUnitOfWorkFactory`).

## API

```pascal
TUnitOfWorkState = (Active, Committed, RolledBack);  // scoped

IUnitOfWork = interface
  procedure Complete;                  // commits immediately
  procedure Rollback;                  // rolls back immediately
  function Connection: TFDConnection;  // only while Active
  function State: TUnitOfWorkState;
end;

IUnitOfWorkFactory = interface
  function Start: IUnitOfWork;         // isolation level of the connection
  function StartWithIsolation(const AIsolationLevel: TFDTxIsolation): IUnitOfWork;
end;

constructor TUnitOfWork.Create(const AConnection: TFDConnection;
  const AIsolationLevel: TFDTxIsolation = xiUnspecified);
constructor TUnitOfWorkFactory.Create(const AConnection: TFDConnection);
```

## Pattern

```pascal
uses
  DX.Data.UnitOfWork.Interfaces,
  DX.Data.UnitOfWork;

begin
  // Always IUnitOfWork, never a class reference: only reference counting
  // guarantees the rollback when the scope is left.
  var LUoW: IUnitOfWork := TUnitOfWork.Create(Connection);
  FCustomers.Insert(LUoW.Connection, LCustomer);
  FOrders.Insert(LUoW.Connection, LOrder);
  LUoW.Complete;
end;  // without Complete (exception): rollback here; the destructor never raises
```

In an `except` handler, check `State` before calling `Rollback`: if the
error came from `Complete`, the unit of work is already `RolledBack` and
`Rollback` would raise `UOW_NOT_ACTIVE`.

```pascal
except
  on E: EDataConstraintException do
  begin
    if LUoW.State = TUnitOfWorkState.Active then
      LUoW.Rollback;
    ShowErrors(E.Errors);
  end;
end;
```

## Behaviour

- `Complete` commits at once. If the commit fails, it rolls back
  (`RolledBack`) and raises the error translated by `TDataErrorHandler`
  (deferred FK → `EDataConstraintException` `DB_FK_VIOLATION`).
- Before the commit, `Complete` checks: state → transaction present → server
  state → open savepoints.
- Backends after a failed statement: SQLite keeps the transaction usable;
  PostgreSQL aborts it (a later COMMIT silently rolls back); SQL Server with
  `XACT_ABORT ON` rolls back and the ODBC driver silently starts a new one.
  `Complete` detects PostgreSQL (`SELECT 1`) and SQL Server
  (`XACT_STATE()`, `CURRENT_TRANSACTION_ID()`) and raises `UOW_TX_ABORTED`.
- The connection's `TxOptions` are saved and restored; without an explicit
  isolation level the connection's applies.
- Begin, commit and rollback are logged through `IDataLogger`.

## Codes

| Code | Class | Meaning |
|---|---|---|
| `UOW_NOT_ACTIVE` | `EDataException` | call after Committed/RolledBack |
| `UOW_NESTED` | `EDataException` | connection already in a transaction — no nesting |
| `UOW_KEEP_CONNECTION_OFF` | `EDataException` | `ResourceOptions.KeepConnection = False` |
| `UOW_INNER_TX_OPEN` | `EDataException` | savepoint still open at `Complete`; all rolled back |
| `UOW_TX_ABORTED` | `EDataException` | server already aborted the transaction |
| `UOW_TX_LOST` | `EDataConnectionException` | transaction ended behind the UoW (direct commit/rollback, lost connection, reconnect) |
| `UOW_SERVER_UNSUPPORTED` | `EDataException` | SQL Server before 2016 |
| `UOW_ROLLBACK_INCOMPLETE` | `EDataException` | transaction still open after a rollback without error |

## Backend notes

- **SQL Server 2016+** required (`CURRENT_TRANSACTION_ID()`); one extra round
  trip at start and per `Complete`.
- **PostgreSQL**: one extra round trip per `Complete`. FireDAC sets the
  isolation level per session and sends nothing for `xiUnspecified`; with
  `StartWithIsolation`, set the connection's isolation level explicitly or the
  session keeps the unit of work's level.
- Inside a unit of work use `TDataErrorHandler.Capture(Connection, …)` (with
  the connection) to catch an expected error without aborting the
  transaction.
- `AutoReconnect`: a statement after a reconnect runs outside the unit of
  work; `Complete` reports `UOW_TX_LOST` but cannot undo it.
