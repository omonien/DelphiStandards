# Delphi Style Guide (EN)

> Version: 2.0
> Author: Olaf Monien
> Updated: 2025-01-03

> Note: This style guide is maintained in both German and English. Keep both documents in sync when making changes.


This guide defines formatting and naming conventions for modern Delphi projects. It aims to improve readability, maintainability, and team consistency.

## Table of Contents

- [1. Formatting](#1-formatting)
- [2. Naming Conventions](#2-naming-conventions)
- [3. Unit Structure](#3-unit-structure)
- [4. Coding Style](#4-coding-style)
- [5. Properties and Getters/Setters](#5-properties-and-getterssetters)
- [6. Events](#6-events)
- [7. Miscellaneous](#7-miscellaneous)
- [8. Modern Delphi Features](#8-modern-delphi-features)
- [9. Documentation](#9-documentation)
- [10. Summary](#10-summary)

---

## 1. Formatting

### 1.1 Indentation

Use 2 spaces per logical block. Avoid tabs as they may render differently across editors.

```pascal
procedure Example;
begin
  DoSomething;
  if Condition then
  begin
    DoSomethingElse;
  end;
end;
```

### 1.2 Line length

- Max 120 characters per line.

The historical default of 80 characters comes from text terminals. In modern projects, 120 chars has become a de facto standard because it strikes a good balance between readability and context. On modern displays, this has become the lived standard for most Delphi developers.

Note: If you use an automatic formatter and the editor's vertical guideline, keep both settings (line length/guideline) in sync to avoid inconsistent wrapping.

### 1.3 Comments

- `//` single-line comments
- `{}` multi-line comments
- `(* *)` temporarily commented-out code
- `///` XML documentation comments

```pascal
// Single-line comment

{ Multi-line
  comment }

(* Temporarily disabled code
   procedure OldMethod;
   begin
     // ...
   end; *)

/// <summary>
/// Documentation comment for a method
/// </summary>
/// <param name="AValue">Parameter description</param>
procedure DocumentedMethod(const AValue: string);
```

### 1.4 Compiler directives

Uppercase inside braces, not indented. Nested directives may be indented for readability.

```pascal
{$IFDEF DEBUG}
  {$IFDEF LOGGING}
  // Debug logging
  {$ENDIF}
// General debug code
{$ENDIF}

{$REGION 'Private Methods'}
// Implementation
{$ENDREGION}
```

### 1.5 Statement syntax

- One statement per line
- `begin`/`end` on their own lines
- Prefer `begin..end` even for single statements, except simple ones like `raise`, `exit`, `continue`, `break`

```pascal
// Preferred - with begin..end
if Condition then
begin
  DoSomething;
end;

// Acceptable for simple statements
if HasError then
  raise Exception.Create('Error');

if not Found then
  Exit;

// Always use begin..end for multiple statements
if UserLoggedIn then
begin
  UpdateLastLogin;
  ShowDashboard;
end;
```

---

## 2. Naming Conventions

PascalCase throughout the codebase (types, methods, variables, constants, parameters).

### 2.1 Methods

- Start with a verb, be descriptive

```pascal
// Procedures (actions)
procedure SaveDocument;
procedure DrawRectangle;
procedure ValidateUserInput;

// Functions (return values)
function GetUserName: string;
function IsValidEmail(const AEmail: string): Boolean;
function CalculateTotalPrice: Currency;

// Avoid vague names
procedure DoSomething;  // Bad
procedure ProcessData;  // Better: ValidateAndSaveData
```

### 2.2 Parameters

- Prefix `A`, use PascalCase
- Use `const` for immutable parameters
- Use `var`/`out` explicitly

```pascal
// Simple parameters
procedure DrawRectangle(AX, AY: Integer);

// const parameters
procedure SaveToFile(const AFileName: string; const AData: TStringList);

// var for output parameters
procedure GetUserInfo(const AUserID: Integer; var AName: string; var AEmail: string);

// out for initialized output parameters
procedure TryParseInteger(const AValue: string; out AResult: Integer; out ASuccess: Boolean);
```

### 2.3 Variables

- Locals: `L` prefix
- Fields: `F` prefix
- Globals: `G` prefix (avoid)
- PascalCase, no type prefixes (except component naming)

```pascal
var
  LUserName: string;
  LCustomerList: TObjectList<TCustomer>;
  LIndex: Integer;
  LFound: Boolean;
```

```pascal
type
  TMyClass = class
  private
    FConnectionString: string;
    FIsConnected: Boolean;
    FCustomers: TObjectList<TCustomer>;
  end;
```

```pascal
// Global variables (avoid!)
var
  GAppTitle: string;
  GLogLevel: Integer;
```

#### 2.3.1 Component names

- Use component type as prefix (PascalCase)
- No `F`/`L`/`G` prefixes on component instances
- Use descriptive names

```pascal
// UI components
ButtonLogin: TButton;
ButtonCancel: TButton;
EditUserName: TEdit;
EditPassword: TEdit;
LabelWelcome: TLabel;
PanelHeader: TPanel;
GridCustomers: TStringGrid;

// Database components
QCustomers: TFDQuery;
QOrders: TFDQuery;
ConnectionMain: TFDConnection;

// Forms and modules
FormMain: TFormMain;
FormSettings: TFormSettings;
DMMain: TDataModule;
```

### 2.4 Constants

- Use `c` for general constants, `sc` for string constants
- Use ALL_CAPS only for system/build related constants

```pascal
// Technical constants
const
  cDefaultTimeout = 5000;
  cMaxRetryCount = 3;
  cBufferSize = 1024;
  cPI = 3.14159265359;

// UI/string constants
const
  scLoginErrorMessage = 'Invalid username or password.';
  scFormCaption = 'My Application';
  scConfirmDelete = 'Do you really want to delete this item?';

// System-wide constants (build-related)
const
  APP_VERSION = '1.2.3';
  BUILD_NUMBER = 12345;
  COMPANY_NAME = 'My Company Ltd';

// Resource strings (localizable)
resourcestring
  rsErrorFileNotFound = 'File not found.';
  rsConfirmExit = 'Do you want to exit the application?';
```

### 2.5 Types and Interfaces

- Types: `T` prefix
- Interfaces: `I` prefix
- Exceptions: `E` prefix

```pascal
type
  // Classes (minimal valid placeholders)
  TCustomer = class end;
  TOrderManager = class end;
  TDatabaseConnection = class end;

  // Interfaces (placeholders)
  ILogger = interface end;
  IDataRepository = interface end;
  IEmailService = interface end;

  // Records (placeholders)
  TPoint3D = record end;
  TCustomerData = record end;

  // Enums (prefer with {$SCOPEDENUMS ON})
  TOrderStatus = (New, Processing, Completed, Cancelled);
  TLogLevel = (Debug, Info, Warning, Error);
```

---

## 3. Unit Structure

- Interface section (public declarations)
- Implementation section (private implementation)
- Initialization/Finalization only when necessary
- Group uses clauses logically

```pascal
unit Customer.Manager;

interface

uses
  // System units
  System.SysUtils, System.Classes, System.Generics.Collections,
  // Database units
  FireDAC.Comp.Client, FireDAC.Stan.Param,
  // Own units
  Customer.Types, Database.Connection;

type
  TCustomerManager = class
  private
    FConnection: TDatabaseConnection;
    FCustomers: TObjectList<TCustomer>;
  public
    constructor Create(AConnection: TDatabaseConnection);
    destructor Destroy; override;
    procedure LoadCustomers;
    function FindCustomer(const AID: Integer): TCustomer;
  end;

implementation

uses
  // Units needed only in implementation
  System.StrUtils, System.DateUtils;

{ TCustomerManager }

constructor TCustomerManager.Create(AConnection: TDatabaseConnection);
begin
  inherited Create;
  FConnection := AConnection;
  FCustomers := TObjectList<TCustomer>.Create(True);
end;

destructor TCustomerManager.Destroy;
begin
  FreeAndNil(FCustomers);
  inherited;
end;

procedure TCustomerManager.LoadCustomers;
begin
  // Implementation
end;

function TCustomerManager.FindCustomer(const AID: Integer): TCustomer;
begin
  // Implementation
  Result := nil;
end;

end.
```

---

## 4. Coding Style

### 4.1 Error handling

- Use `try..finally` to release resources
- Prefer `FreeAndNil` over `.Free`
- Use `try..except` only when you can handle the error meaningfully

```pascal
var
  LQuery: TFDQuery;
  LList: TObjectList<TObject>;
begin
  LQuery := nil;
  LList := nil;
  try
    LQuery := TFDQuery.Create(nil);
    LList := TObjectList<TObject>.Create(True);
    // Use objects
  finally
    FreeAndNil(LQuery);
    FreeAndNil(LList);
  end;
end;
```

Additional rules:
- Never use empty `except` blocks

```pascal
// Simple example
LObject := TObject.Create;
try
  // use object
finally
  FreeAndNil(LObject);
end;

// Exception handling
try
  RiskyOperation;
except
  on E: ESpecificException do
  begin
    LogError(E.Message);
    raise; // rethrow if needed
  end;
  on E: Exception do
  begin
    LogError('Unexpected error: ' + E.Message);
    // handle or rethrow
  end;
end;
```

### 4.2 Branches and loops

- Prefer `begin..end` blocks for clarity, even on single-line branches

```pascal
if UserLoggedIn then
begin
  ShowDashboard;
end
else
begin
  ShowLoginScreen;
end;

case DayOfTheWeek(Date) of // 1=Monday .. 7=Sunday
  1: DoMondayRoutine;
  2: DoTuesdayRoutine;
  // ...
end;

for I := 1 to 10 do
begin
  if SomeCondition then
    Break; // no begin..end needed here
end;
```

### 4.3 Numerics & types

- Avoid direct float equality (`=`); use an epsilon
- Use `Double`/`Single` for floats where exact precision is not critical
- Use `Currency` or `TBcd` for monetary calculations
- Use `Variant` only when technically required (e.g., COM)

---

## 5. Properties and Getters/Setters

- Only add getters/setters if additional logic is needed
- Fields (`F...`) should be `private`; getters/setters ideally `protected`

```pascal
type
  TMyClass = class
  private
    FName: string;
    FAge: Integer;
  protected
    function GetName: string; virtual;
    procedure SetName(const AValue: string); virtual;
    function GetDisplayName: string; virtual;
  public
    property Name: string read GetName write SetName;
    property Age: Integer read FAge write FAge;
    property DisplayName: string read GetDisplayName;
  end;

implementation

function TMyClass.GetName: string;
begin
  Result := FName;
end;

procedure TMyClass.SetName(const AValue: string);
begin
  if FName <> AValue then
  begin
    FName := Trim(AValue);
    // Additional validation or notification
  end;
end;

function TMyClass.GetDisplayName: string;
begin
  Result := Format('%s (%d years)', [FName, FAge]);
end;
```

---

## 6. Events

- Event names start with `On`
- Handler methods should start with `Do...` and delegate to business logic

```pascal
// Event handler delegating to logic
procedure TForm1.ButtonLoginClick(Sender: TObject);
begin
  DoLogin;
end;

procedure TForm1.ButtonCancelClick(Sender: TObject);
begin
  DoCancel;
end;

// Business logic in separate methods
procedure TForm1.DoLogin;
begin
  if ValidateLoginData then
  begin
    AuthenticateUser;
    ShowMainForm;
  end
  else
  begin
    ShowMessage(scLoginErrorMessage);
  end;
end;

procedure TForm1.DoCancel;
begin
  if ConfirmExit then
    Close;
end;
```

---

## 7. Miscellaneous

### 7.1 WITH constructs

Avoid the `with` statement.

```pascal
// Avoid
with Customer do
begin
  Name := 'Max';
  Address := 'Example Street';
end;

// Prefer
Customer.Name := 'Max';
Customer.Address := 'Example Street';
```

### 7.2 Record types

- Use records for simple, self-contained data structures
- Prefix record names with `T`

```pascal
type
  TPoint = record
    X, Y: Integer;
  end;
```

### 7.3 Enumerations

- Prefix enum types with `T`
- Prefer dot-notation with `{$SCOPEDENUMS ON}`

```pascal
type
  TOrderStatus = (New, Processing, Completed);
var
  LStatus: TOrderStatus;
begin
  LStatus := TOrderStatus.New;
end;
```

### 7.4 Thread safety

Prefer `TMonitor` for synchronization in simple scenarios.

```pascal
procedure TMyObject.AddCustomer(const ACustomer: TCustomer);
begin
  TMonitor.Enter(Self);
  try
    FCustomers.Add(ACustomer);
  finally
    TMonitor.Exit(Self);
  end;
end;
```

---

## 8. Modern Delphi Features

### 8.1 Generics

Use generics for type-safe collections and reusable algorithms.

```pascal
var
  LCustomers: TObjectList<TCustomer>;
  LNames: TList<string>;
  LLookup: TDictionary<string, TCustomer>;

function FindItem<T>(const AList: TList<T>; const APredicate: TFunc<T, Boolean>): T;
var
  LItem: T;
begin
  for LItem in AList do
  begin
    if APredicate(LItem) then
      Exit(LItem);
  end;
  Result := Default(T);
end;
```

### 8.2 Anonymous methods

Anonymous methods are useful for short, local functionality.

```pascal
var
  LButton: TButton;
begin
  LButton := TButton.Create(Self);
  LButton.OnClick := procedure(Sender: TObject)
    begin
      ShowMessage('Button clicked');
    end;
end;
```

### 8.3 Inline variables (Delphi 10.3+)

Declare variables at the point of use for better readability.

```pascal
// Traditional
procedure ProcessData;
var
  LIndex: Integer;
  LCustomer: TCustomer;
begin
  for LIndex := 0 to CustomerList.Count - 1 do
  begin
    LCustomer := CustomerList[LIndex];
    // ...
  end;
end;

// With inline variables
procedure ProcessData;
begin
  for var LIndex := 0 to CustomerList.Count - 1 do
  begin
    var LCustomer := CustomerList[LIndex];
    // ...
  end;

  var LResult := CalculateSomething;
  if LResult > 0 then
    ProcessResult(LResult);
end;
```

### 8.4 Multiline strings (Delphi 12+)

Use triple quotes `'''` with opening and closing quotes on their own lines. The closing indentation defines the base indentation; leading spaces up to that level are removed. The last newline before the closing quotes is omitted.

```pascal
// Traditional - hard to read
const
  SQL_QUERY = 'SELECT c.id, c.name, c.email, o.order_date ' +
              'FROM customers c ' +
              'LEFT JOIN orders o ON c.id = o.customer_id ' +
              'WHERE c.active = 1 ' +
              'ORDER BY c.name';

// Multiline strings (Delphi 12+)
const
  SQL_QUERY = '''
SELECT c.id, c.name, c.email, o.order_date
FROM customers c
LEFT JOIN orders o ON c.id = o.customer_id
WHERE c.active = 1
ORDER BY c.name
''';
```

### 8.5 Attributes

Use attributes for metadata and configuration (examples are illustrative; actual attributes depend on your frameworks).

```pascal
type
  [Table('customers')]
  TCustomer = class
  private
    [Column('id', True)] // True = Primary Key
    FID: Integer;

    [Column('name')]
    [Required]
    [MaxLength(100)]
    FName: string;

    [Column('email')]
    [Email]
    FEmail: string;
  public
    property ID: Integer read FID write FID;
    property Name: string read FName write FName;
    property Email: string read FEmail write FEmail;
  end;
```

---

## 9. Documentation

Use XML documentation comments for public APIs.

```pascal
/// <summary>
/// Calculates the distance between two points.
/// </summary>
/// <param name="APoint1">First point</param>
/// <param name="APoint2">Second point</param>
/// <returns>Distance as a Double</returns>
/// <exception cref="EArgumentException">
/// Raised when one of the points is nil.
/// </exception>
function CalculateDistance(const APoint1, APoint2: TPoint): Double;
```

---

## 10. Summary

- Consistent formatting, naming, and structure
- Clear unit organization and separation of concerns
- Modern features: generics, anonymous methods, inline variables (10.3+), multiline strings (12+), attributes

---

## License

MIT License
https://opensource.org/licenses/MIT

