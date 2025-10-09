# Delphi Style Guide (EN)

> Version: 2.1
> Author: Olaf Monien
> Updated: 2025-10-08

> Note: This style guide is maintained in both German and English. Keep both documents in sync when making changes.


This guide defines formatting and naming conventions for modern Delphi projects. It aims to improve readability, maintainability, and team consistency.

---

## Quick Start - Essential Rules at a Glance

New to the project? Here are the essential rules:

### **Naming**
```pascal
// Variables
var
  LCustomer: TCustomer;        // Local: L prefix

type
  TMyClass = class
  private
    FName: string;             // Field: F prefix
  end;

// Parameters
procedure DoSomething(const AValue: string);  // Parameter: A prefix

// Loop counters - Exception!
for var i := 0 to 10 do       // Lowercase, no prefix

// Constants
const
  cMaxRetries = 3;             // Technical: c prefix
  scErrorMessage = 'Error';    // String: sc prefix
```

### **Types**
```pascal
type
  TCustomer = class end;           // Class: T prefix
  ILogger = interface end;         // Interface: I prefix
  TPoint = record end;             // Record: T prefix, NO F prefix for fields!
  TFileUtils = class sealed end;   // Utility class: sealed
  TStringHelper = record helper for string end;  // Helper: only for real helpers!
```

### **Error Handling**
```pascal
// Free resources
LObject := TObject.Create;
try
  // Use object
finally
  FreeAndNil(LObject);  // Always FreeAndNil instead of .Free
end;

// Multiple objects
LQuery := nil;
LList := nil;
try
  LQuery := TFDQuery.Create(nil);
  LList := TList<string>.Create;
finally
  FreeAndNil(LQuery);
  FreeAndNil(LList);
end;
```

### **Formatting**
- **2 spaces** indentation
- **120 characters** max line length
- `begin..end` always on separate lines
- Prefer inline variables (from Delphi 10.3+)

### **Collections**
```pascal
// Fixed size → TArray<T>
function GetNames: TArray<string>;

// Dynamic list → TList<T>
var LNumbers: TList<Integer>;

// Objects with ownership → TObjectList<T>
var LCustomers: TObjectList<TCustomer>;
```

### **Unit Names (Namespace Hierarchy)**
```pascal
// Forms end with .Form.pas
unit Main.Form;                    // TFormMain / FormMain
unit Customer.Details.Form;        // TFormCustomerDetails / FormCustomerDetails

// Data modules end with .DM.pas
unit Main.DM;                      // TDMMain / DMMain
unit Customer.Details.DM;          // TDMCustomerDetails / DMCustomerDetails
```

### **Documentation**
```pascal
/// <summary>
/// Calculates the sum of two numbers
/// </summary>
function Add(const AValue1, AValue2: Integer): Integer;
```

**→ See full documentation below for details.**

---

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
- **Exception**: Simple loop counters may use lowercase single letters (`i`, `j`, `k`) without prefix

```pascal
var
  LUserName: string;
  LCustomerList: TObjectList<TCustomer>;
  LIndex: Integer;
  LFound: Boolean;

// Loop counters - exception to the rule
for var i := 0 to 10 do
begin
  // Simple loop counter without L prefix
end;

// Nested loops
for var i := 0 to Rows - 1 do
begin
  for var j := 0 to Cols - 1 do
  begin
    Matrix[i, j] := 0;
  end;
end;
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

**Exception: Unit-internal global variables in implementation section**

Global variables in the `implementation` section are acceptable for unit-internal singletons or state management:

```pascal
unit MyService;

interface

type
  TMyService = class
    class procedure Initialize;
    class procedure Finalize;
  end;

implementation

var
  GServiceInstance: TMyService;  // Unit-internal, not visible from outside
  GInitialized: Boolean = False;

class procedure TMyService.Initialize;
begin
  if not GInitialized then
  begin
    GServiceInstance := TMyService.Create;
    GInitialized := True;
  end;
end;

class procedure TMyService.Finalize;
begin
  FreeAndNil(GServiceInstance);
  GInitialized := False;
end;

end.
```

**Advantages:**
- Not visible from outside (encapsulation)
- Ideal for unit singletons
- Avoids global namespace pollution

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

**Note:** For naming conventions of related units, see [Section 3.1 - Unit Naming Conventions and Namespace Hierarchy](#31-unit-naming-conventions-and-namespace-hierarchy).

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

  // Sealed Classes (utility classes without instances)
  TPathUtils = class sealed end;
  TStringUtils = class sealed end;

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

### 3.1 Unit Naming Conventions and Namespace Hierarchy

Modern Delphi projects should use a consistent namespace hierarchy with context-based structure. This significantly improves organization, maintainability, and clarity in larger projects.

**Core Principles:**
- Unit names follow a hierarchical structure with dot notation
- File name matches the unit name (e.g., `Main.Form.pas` for `unit Main.Form`)
- Forms end with `.Form.pas`
- Data modules end with `.DM.pas`
- Nested hierarchies are allowed and recommended

**Examples for Main Components:**

```pascal
// Main form
unit Main.Form;
// File: Main.Form.pas
// Class: TFormMain
// Instance: FormMain

// Main data module (usually contains the central DB connection)
unit Main.DM;
// File: Main.DM.pas
// Class: TDMMain
// Instance: DMMain
```

**Examples for Nested Hierarchies:**

```pascal
// Customer details form
unit Customer.Details.Form;
// File: Customer.Details.Form.pas
// Class: TFormCustomerDetails
// Instance: FormCustomerDetails

// Customer details data module (only queries for customer details)
unit Customer.Details.DM;
// File: Customer.Details.DM.pas
// Class: TDMCustomerDetails
// Instance: DMCustomerDetails

// Additional examples
unit Customer.List.Form;        // Customer list
unit Customer.Edit.Form;        // Customer editing
unit Reports.Sales.Form;        // Sales reports
unit Reports.Sales.DM;          // Data module for sales reports
unit Settings.Database.Form;    // Database settings
```

**Advantages of this Structure:**
- Clear association of related units
- Better overview in large projects
- Easier to find related components
- Avoids naming conflicts
- Logical grouping in Project Manager and uses clauses

**Naming Conventions for Classes and Instances:**

| Unit Name | File Name | Class Name | Instance Name |
|-----------|-----------|------------|---------------|
| `Main.Form` | `Main.Form.pas` | `TFormMain` | `FormMain` |
| `Main.DM` | `Main.DM.pas` | `TDMMain` | `DMMain` |
| `Customer.Details.Form` | `Customer.Details.Form.pas` | `TFormCustomerDetails` | `FormCustomerDetails` |
| `Customer.Details.DM` | `Customer.Details.DM.pas` | `TDMCustomerDetails` | `DMCustomerDetails` |

### 3.2 Example of Complete Unit Structure

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

**Exception: Defensive programming in critical systems**

In critical systems (e.g., exception handlers, logging, cleanup code), it may be necessary to suppress errors to prevent recursion or system crashes:

```pascal
// Exception handler must not throw exceptions itself
procedure LogException(const E: Exception);
begin
  try
    WriteToLogFile(E.Message);
  except
    // Silently ignore - logging must not fail
    // Alternative: Fallback to OutputDebugString
  end;
end;

// Cleanup code in finalization
finalization
  try
    if Assigned(GResolver) then
      FreeAndNil(GResolver);
  except
    // Silently ignore - finalization must complete
  end;
end.
```

**Important:** Such `except` blocks should:
- Have a comment explaining why errors are suppressed
- Only be used when absolutely necessary
- Preferably have a fallback mechanism

### 4.2 Branches and loops

- Prefer `begin..end` blocks for clarity, even on single-line branches
- Loop counters may use lowercase single letters (`i`, `j`, `k`) without `L` prefix

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

// Simple loop counter
for var i := 1 to 10 do
begin
  if SomeCondition then
    Break; // no begin..end needed here
end;

// Traditional declaration
var
  i: Integer;
begin
  for i := 0 to List.Count - 1 do
  begin
    ProcessItem(List[i]);
  end;
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
- **Record fields have NO prefixes** (no `F`, `L`, `G`) – they are always public
- Avoid methods or complex logic inside records unless using `record helpers` or `record with methods`
- Records are ideal for DTOs, geometry types, or simple value objects

```pascal
type
  TPoint = record
    X, Y: Integer;  // No F prefix for records!
  end;

  TStackFrameInfo = record
    ModuleName: string;
    ProcName: string;
    FileName: string;
    Line: Integer;
    Address: Pointer;
  end;
```

**Comparison Class vs. Record:**

```pascal
// Class - private fields with F prefix
type
  TCustomer = class
  private
    FName: string;      // F prefix for private fields
    FAge: Integer;
  public
    property Name: string read FName write FName;
    property Age: Integer read FAge write FAge;
  end;

// Record - public fields without prefix
type
  TCustomerData = record
    Name: string;       // No prefix - always public
    Age: Integer;
  end;
```

### 7.2a Sealed Classes for Utility Functions

For pure utility classes without instances and state, use `class sealed` with only `class` methods.

**When to use `sealed class` instead of `record`:**

- **Record**: For data structures (DTOs, value objects)
  ```pascal
  type
    TPoint = record
      X, Y: Integer;
    end;
  ```

- **Sealed Class**: For utility functions without data
  ```pascal
  type
    TPathUtils = class sealed
      class function FileExists(const APath: string): Boolean;
      class procedure DeleteFile(const APath: string);
    end;
  ```

**Advantages of `sealed` for utility classes:**

- Prevents meaningless inheritance
- Communicates design intent: "No instances, only functions"
- Enables compiler optimizations
- Follows best practices from other languages (C# `static class`, Java `final class`)

**Comparison:**

| Aspect | Record | Sealed Class | Regular Class |
|--------|--------|--------------|---------------|
| Purpose | Data structure | Utility functions | Objects with state |
| Instances | Value semantics | None (class methods only) | Reference semantics |
| Inheritance | No | No (sealed) | Yes (possible) |
| Example | `TPoint`, `TRect` | `TPathUtils`, `TDXStacktrace` | `TCustomer`, `TOrder` |

### 7.3 Class and Record Helpers

Class and record helpers extend existing types with additional methods without modifying the original type. Use the suffix `Helper` **only** for actual class and record helpers.

**Naming convention:**
- Format: `T<TypeName>Helper`
- The word "Helper" is **reserved** for class and record helpers only
- Do NOT use "Helper" for sealed utility classes

```pascal
// Correct - Record Helper
type
  TPointHelper = record helper for TPoint
    function Distance(const AOther: TPoint): Double;
    function ToString: string;
  end;

// Correct - Class Helper
type
  TStringListHelper = class helper for TStringList
    procedure SaveToFileUTF8(const AFileName: string);
    function ContainsText(const AText: string): Boolean;
  end;

// WRONG - Not a helper, just a utility class
type
  TFileHelper = class sealed  // Should be TFileUtils or similar
    class function FileExists(const APath: string): Boolean;
  end;

// Correct - Sealed utility class
type
  TFileUtils = class sealed
    class function FileExists(const APath: string): Boolean;
    class procedure DeleteFile(const APath: string);
  end;
```

**When to use helpers:**
- Extending RTL/VCL/FMX types without inheritance
- Adding convenience methods to records
- Backward compatibility when you can't modify the original type

**Important notes:**
- Only one helper can be active for a type in a given scope
- Helpers cannot add fields, only methods
- Helper methods have access to private members of the extended type

### 7.4 Enumerations

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

### 7.5 Thread safety

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

#### 8.1.1 TArray<T> vs. TList<T> vs. TObjectList<T>

Choose the right collection type based on your use case:

**TArray<T>** - For collections with fixed or rarely changing size:
```pascal
type
  TStacktrace = TArray<TStackFrameInfo>;  // Fixed size after capture

function GetTopCustomers: TArray<TCustomer>;  // Return value

var
  LNames: TArray<string>;
begin
  SetLength(LNames, 3);
  LNames[0] := 'Alice';   // O(1) - very fast, no reallocation
  LNames[1] := 'Bob';
  LNames[2] := 'Charlie';
  LNames[0] := 'John';    // Changing elements is O(1)
end;
```

**Advantages:**
- Low memory overhead
- Very fast index access and element modification (O(1))
- Ideal for return values and fixed-size collections
- No memory management needed (automatically freed)
- Elements can be modified efficiently in-place

**TList<T>** - For dynamic lists of value types:
```pascal
var
  LNumbers: TList<Integer>;
  LNames: TList<string>;
begin
  LNumbers := TList<Integer>.Create;
  try
    LNumbers.Add(42);
    LNumbers.Add(100);
  finally
    FreeAndNil(LNumbers);
  end;
end;
```

**Advantages:**
- Dynamic add/remove
- Built-in sorting and searching
- Ideal for value types (Integer, String, Records)

**TObjectList<T>** - For lists of objects with ownership:
```pascal
var
  LCustomers: TObjectList<TCustomer>;
begin
  LCustomers := TObjectList<TCustomer>.Create(True);  // True = OwnsObjects
  try
    LCustomers.Add(TCustomer.Create('Max'));
    // Objects are automatically freed
  finally
    FreeAndNil(LCustomers);
  end;
end;
```

**Advantages:**
- Automatic memory management for objects (when OwnsObjects = True)
- Prevents memory leaks
- Ideal for object collections

**Decision guide:**

| Criterion | TArray<T> | TList<T> | TObjectList<T> |
|-----------|-----------|----------|----------------|
| Size changes | Rarely (SetLength) | Frequently (Add/Delete) | Frequently (Add/Delete) |
| Element modification | Very fast (O(1)) | Fast (O(1)) | Fast (O(1)) |
| Content | Any type | Value types | Objects |
| Ownership | N/A | N/A | Yes (OwnsObjects) |
| Memory overhead | Minimal | Medium | Medium |
| Use case | Fixed-size collections, return values | Dynamic lists | Object collections |

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

Declare variables at the point of use for better readability. For loops, prefer inline declaration.

```pascal
// Traditional
procedure ProcessData;
var
  i: Integer;
  LCustomer: TCustomer;
begin
  for i := 0 to CustomerList.Count - 1 do
  begin
    LCustomer := CustomerList[i];
    // ...
  end;
end;

// With inline variables (preferred from Delphi 10.3+)
procedure ProcessData;
begin
  for var i := 0 to CustomerList.Count - 1 do
  begin
    var LCustomer := CustomerList[i];
    // ...
  end;

  // Also useful for other variables
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

Use XML documentation comments (`///`) consistently for **all** public APIs:
- All public classes, records, and interfaces
- All public methods and functions
- All public properties
- All public types and constants

**Documentation levels:**
- **Minimum:** `<summary>` for all public elements
- **Optimal:** Additionally include `<param>` for all parameters, `<returns>` for functions with return values, `<exception>` for documented exceptions, and `<remarks>` for additional notes when helpful

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

/// <summary>
/// Represents a customer in the system
/// </summary>
/// <remarks>
/// This class encapsulates all customer-related data and operations.
/// Use the CreateCustomer factory method for instantiation.
/// </remarks>
type
  TCustomer = class
  private
    FID: Integer;
    FName: string;
  public
    /// <summary>Unique customer ID</summary>
    property ID: Integer read FID write FID;

    /// <summary>Full name of the customer</summary>
    property Name: string read FName write FName;
  end;

/// <summary>
/// Information about a single stack frame
/// </summary>
type
  TStackFrameInfo = record
    /// <summary>Name of the module (EXE/DLL)</summary>
    ModuleName: string;
    /// <summary>Procedure/function name</summary>
    ProcName: string;
    /// <summary>Source file path</summary>
    FileName: string;
    /// <summary>Line number in source file</summary>
    Line: Integer;
  end;
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

