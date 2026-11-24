# Adding a table — Orders on the trivial VCL demo

Guided walkthrough. Start from a working `demo/VCL.Trivial/` (entity,
data module, form, project). After each step, build and run before
moving on.

> `demo/VCL.Trivial/` is **not** part of this repository (the files
> `Trivial.Entities.pas`, `Trivial.DM.Main` and `Database/trivial_demo.db`
> named below do not exist here). The walkthrough applies the same way to
> any VCL project with an entity unit and a data module; the demo in this
> repository has comparable files in `demo/Demo.Customer.pas`,
> `demo/Demo.Order.pas`, `demo/Demo.Main.DataModule.pas` and
> `demo/Demo.Main.Seed.pas`. The snippets below are excerpts of such a
> project, without the surrounding `uses` clauses.

Delete `Database/trivial_demo.db` when the schema changes — `CREATE TABLE
IF NOT EXISTS` will not add columns to an existing file.

---

## 1. Define `TOrder` in `Trivial.Entities.pas`

Add the class next to `TCustomer`. Attributes on **private fields**.
`CUSTOMERS` must exist before `ORDERS` (foreign key).

```pascal
  [Table('ORDERS')]
  TOrder = class
  private
    [Column('ID', TColumnType.BigInt)]
    [PrimaryKey]
    [AutoIncrement]
    FID: Int64;

    [Column('CUSTOMER_ID', TColumnType.BigInt)]
    [Required]
    [ForeignKey('CUSTOMERS', 'ID')]
    FCustomerID: Int64;

    [Column('ORDER_DATE', TColumnType.Date)]
    [Required]
    FOrderDate: TDateTime;

    [Column('AMOUNT', TColumnType.Decimal, 10, 2)]
    [Required]
    FAmount: Currency;
  public
    property ID: Int64 read FID write FID;
    property CustomerID: Int64 read FCustomerID write FCustomerID;
    property OrderDate: TDateTime read FOrderDate write FOrderDate;
    property Amount: Currency read FAmount write FAmount;
  end;
```

`Currency` is an exact type. For more than four decimal places declare the
field as `TBcd` (`Data.FmtBcd`) instead; a `Double` field would be floating
point.

`[ForeignKey('CUSTOMERS', 'ID')]` is the real attribute
(`DX.Data.Schema.Attributes`). Verify: the unit still compiles.

---

## 2. Register it in `CreateSchema` — referenced table first

In `Trivial.DM.Main`, `CreateSchema`:

```pascal
procedure TDMMain.CreateSchema;
var
  LBuilder: TSQLiteSchemaBuilder;
begin
  LBuilder := TSQLiteSchemaBuilder.Create(ConnectionMain);
  try
    LBuilder.CreateTable(TSchemaReader.ReadTableDef<TCustomer>);
    LBuilder.CreateTable(TSchemaReader.ReadTableDef<TOrder>);
  finally
    LBuilder.Free;
  end;
end;
```

Customer **before** order, or the FK has nothing to point at.

Verify: delete `Database/trivial_demo.db`, run, then:

```python
import sqlite3
c = sqlite3.connect(r'Database/trivial_demo.db')
print(c.execute("SELECT name FROM sqlite_master WHERE type='table'").fetchall())
```

Expected: `CUSTOMERS` and `ORDERS`.

---

## 3. Extend `SeedIfEmpty`

After the customer inserts, insert orders for those customers. Use
`TEntityMapper.MapToParams<TOrder>` and the IDs SQLite just created
(`last_insert_rowid` or a follow-up `SELECT ID FROM CUSTOMERS`).

A simple, readable version — three orders for customer ID 1, one for 2:

```pascal
    LInsert.SQL.Text :=
      'INSERT INTO ORDERS (CUSTOMER_ID, ORDER_DATE, AMOUNT) ' +
      'VALUES (:CUSTOMER_ID, :ORDER_DATE, :AMOUNT)';
    // Example row:
    LOrder := TOrder.Create;
    try
      LOrder.CustomerID := 1;
      LOrder.OrderDate := EncodeDate(2026, 1, 15);
      LOrder.Amount := 120.50;
      TEntityMapper.MapToParams<TOrder>(LOrder, LInsert);
      LInsert.ExecSQL;
    finally
      LOrder.Free;
    end;
```

Guard the whole seed with the existing `COUNT(*) FROM CUSTOMERS` check
(empty database ⇒ seed both tables). Do not seed orders if customers
were already present.

Verify: delete the database, run, `SELECT COUNT(*) FROM ORDERS` is > 0.

---

## 4. Add `QOrders` / `DSOrders` to the data module

On `TDMMain` (DFM + class):

- `QOrders: TFDQuery` — `Connection = ConnectionMain`
- SQL: `SELECT * FROM ORDERS WHERE CUSTOMER_ID = :ID ORDER BY ID`
- `MasterSource = DSCustomers`, `MasterFields = ID`
- `DSOrders: TDataSource` — `DataSet = QOrders`

Open the detail in `Configure` **after** `QCustomers.Open` (a
MasterSource-linked detail does not auto-open with its master):

```pascal
  QCustomers.Open;
  if not QOrders.Active then
    QOrders.Open;
```

Wire `QOrders.BeforePost` the same way as customers (safety net). Build
the order validator once in `DataModuleCreate`:

```pascal
FOrderValidator := TValidatorFactory.CreateFor<TOrder>
  .AddRule(TRangeRule.Create('AMOUNT', 0.01, 1000000));
```

Verify: run, select customer 1, the detail query returns that customer's
orders (inspect with the debugger or a second grid in the next step).

---

## 5. Add the detail grid to the form

Below the customers grid (splitter + `TDBGrid`), bind:

```pascal
GridOrders.DataSource := DMMain.DSOrders;
```

Do not put SQL in the form.

Verify: clicking a customer refreshes the orders grid via FireDAC
master-detail.

---

## 6. Validator on `QOrders.BeforePost`

Same pattern as customers: map the current row to `TOrder` with a
TField-direct helper. `TEntityMapper.MapRow` is meant for fetched rows; on a
closed or empty dataset it raises `EDataMappingException` (`MAPPER_NO_ROW`).
Call `FOrderValidator.Validate`. On failure, raise
`EDataValidationException` (for example via `LResult.EnsureSuccess`) so the
post is aborted.

Optionally wire `TField.OnValidate` on `QOrders.Fields` after Open,
filtering `FOrderValidator.Rules` by `FieldName`.

Verify: enter `AMOUNT = -1` and post — dialog, row stays editable.

---

## 7. End-to-end check

1. Delete `Database/trivial_demo.db`.
2. Run. Both grids populate; the detail follows the master.
3. Append an order with a negative amount, post — rejected, no new row
   in SQLite:

```python
import sqlite3
c = sqlite3.connect(r'Database/trivial_demo.db')
print('orders:', c.execute('SELECT COUNT(*) FROM ORDERS').fetchone()[0])
print('negative:', c.execute('SELECT COUNT(*) FROM ORDERS WHERE AMOUNT < 0').fetchone()[0])
```

Expected: seed count unchanged, `negative: 0`.

That is the same claim as the customer grid: schema once, validation
before SQL.
