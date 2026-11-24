# Delphi Coding Standards Reference

Agent digest of the [Delphi Style Guide](../../../../../Delphi%20Style%20Guide%20EN.md) in this repository
([online](https://raw.githubusercontent.com/omonien/DelphiStandards/master/Delphi%20Style%20Guide%20EN.md)). The Style Guide is authoritative; if this digest
disagrees with it, follow the Style Guide.

---

## Fixed vs. Configurable Rules

Some rules are Delphi language/IDE requirements and are **non-negotiable**. Others are style
preferences that can be overridden via `delphi-project.json`.

### Fixed Rules (Never Configurable)

These rules exist because of Delphi IDE requirements, not style preference:

| Rule | Reason |
|------|--------|
| UTF-8 BOM on `.pas`/`.dpr`/`.dpk` | Delphi IDE requires BOM to recognize UTF-8; without it non-ASCII chars are misread |
| CRLF line endings on all source files | Delphi parser and form designer fail silently on LF-only files |
| `$(Platform)\$(Config)` output paths | Required for multi-platform builds; hard-coded paths break cross-platform compilation |
| No absolute paths in DPROJ search paths | Team members have different installation paths; absolute paths break other developers' builds |
| `#decimal` notation for special chars in DFM/FMX | Required by DFM binary format; raw UTF-8 causes corruption |

### Configurable Rules (via `delphi-project.json`)

See `assets/delphi-project.json` for the full schema and defaults.

| Rule | Config Key | Default |
|------|-----------|---------|
| Local variable prefix | `naming.localVarPrefix` | `L` |
| Field prefix | `naming.fieldPrefix` | `F` |
| Parameter prefix | `naming.paramPrefix` | `A` |
| Class type prefix | `naming.classPrefix` | `T` |
| Interface type prefix | `naming.interfacePrefix` | `I` |
| Indentation size | `formatting.indentSize` | `2` |
| Maximum line length | `formatting.maxLineLength` | `120` |
| Executable output path pattern | `paths.outputPath` | `$(Platform)/$(Config)` |
| DCU output path pattern | `paths.dcuPath` | `$(Platform)/$(Config)/dcu` |
| Source directory | `paths.sourceDir` | `src` |
| Test directory | `paths.testDir` | `tests` |
| Unit header author | `header.author` | `Team Name` |
| Unit header license | `header.license` | `MIT` |
| Test framework | `testing.framework` | `DUnitX` |
| DUnitX as submodule | `testing.asSubmodule` | `true` |

---

## 1. File Encoding

**UTF-8 with BOM** for all `.pas`, `.dpr`, `.dpk` files.

The BOM is the byte sequence `EF BB BF` at the very start of the file. Python writes this with
`encoding='utf-8-sig'`. When the Delphi IDE opens a file without BOM, it assumes the system
default code page (usually Windows-1252), causing silent data corruption for non-ASCII characters.

Verify and fix with:
```bash
python scripts/encoding_check.py src/ --fix
```

---

## 2. Line Endings

**CRLF** (`\r\n`) everywhere. This is enforced at the VCS level by `.gitattributes`:

```gitattributes
*.pas text eol=crlf
*.dpr text eol=crlf
*.dpk text eol=crlf
*.dfm text eol=crlf
*.fmx text eol=crlf
*.dproj text eol=crlf
```

When generating files programmatically, always open for writing with `newline='\r\n'`.

---

## 3. Unit Header

Every `.pas` file starts with an XML Doc header block (after the UTF-8 BOM):

```pascal
/// <summary>
///   Brief one-line description of the unit.
/// </summary>
/// <remarks>
///   Additional details, design decisions, or usage notes.
///   Can be multi-line.
/// </remarks>
/// <author>Author Name</author>
/// <created>YYYY-MM-DD</created>
/// <modified>YYYY-MM-DD</modified>
/// <license>MIT</license>
unit My.Unit.Name;
```

The `author` and `license` values come from `delphi-project.json` when present.

---

## 4. Naming Conventions

### Type Prefixes

| Element | Prefix | Example |
|---------|--------|---------|
| Class | `T` | `TCustomer`, `TOrderManager` |
| Interface | `I` | `ICustomer`, `IOrderRepository` |
| Record | `T` | `TPoint3D`, `TApiResponse` |
| Exception | `E` | `ECustomerNotFound`, `EInvalidOperation` |
| Enumeration | `T` | `TOrderStatus`, `TPaymentMethod` |

### Variable Prefixes

| Scope | Prefix | Example |
|-------|--------|---------|
| Method parameter | `A` | `ACustomerName: string` |
| Local variable | `L` | `LCustomer: TCustomer` |
| Class field | `F` | `FCustomerList: TList<TCustomer>` |
| Global variable | `G` | `GAppConfig: TConfig` (avoid globals) |
| Loop counter | none | `i`, `j`, `k` (exception to the rule) |

### Constant Prefixes

| Type | Prefix | Example |
|------|--------|---------|
| General constant | `c` | `cMaxRetries = 3` |
| String constant | `sc` | `scErrorMessage = 'Error'` |
| Resource string | `rs` | `rsWelcomeText` |
| System-level | ALL_CAPS | `APP_VERSION = '1.0'` |

### Unit Names

Use dot-notation for hierarchical organization:
- `Customer.Repository.pas` — customer repository
- `Main.Form.pas` — main form
- `Order.Details.Form.pas` — order details form
- `Customer.DM.pas` — customer data module

### Component Names

Component type as PascalCase prefix, no `F`/`L` prefixes on visual components:
```delphi
ButtonLogin: TButton;
EditUserName: TEdit;
GridCustomers: TDBGrid;
LabelStatus: TLabel;
```

### Enumeration Members

Prefix the enum type with `T`; members have no prefix. Use `{$SCOPEDENUMS ON}` and dot notation
(Style Guide 7.4):
```delphi
{$SCOPEDENUMS ON}
TOrderStatus = (New, Pending, Shipped, Delivered, Cancelled);
LStatus := TOrderStatus.Pending;
```

---

## 5. Code Formatting

### Indentation

2 spaces. No tabs. This applies to `begin`/`end` blocks, `case` branches, and continuation lines.

```delphi
procedure TCustomer.UpdateAddress(AStreet: string; ACity: string);
var
  LAddress: TAddress;
begin
  LAddress := TAddress.Create(AStreet, ACity);
  try
    FAddressRepository.Save(LAddress);
  finally
    FreeAndNil(LAddress);
  end;
end;
```

### Line Length

120 characters maximum. Break long parameter lists and expressions across lines, aligning with
the opening parenthesis:

```delphi
LResult := SomeFunction(
  AFirstParameter,
  ASecondParameter,
  AThirdParameter
);
```

### begin/end Placement

`begin` and `end` always on their own lines (Style Guide 1.5).

**Procedures and functions:**

```delphi
procedure TCustomer.Save;
begin
  // ...
end;
```

**Control structures:** `begin` on a new line:

```delphi
if LCondition then
begin
  DoSomething;
end
else
begin
  DoOtherThing;
end;

for i := 0 to LList.Count - 1 do
begin
  ProcessItem(LList[i]);
end;
```

Single-statement bodies may omit `begin`/`end` only for very short `if` bodies:

```delphi
if not Assigned(LCustomer) then
  Exit;
```

### Parameters

- Use `const` for string, interface, and record parameters that are not modified
- Use `var` or `out` explicitly for output parameters
- `out` for parameters that are always set (not read before write); `var` otherwise

```delphi
procedure Process(const AName: string; var ACount: Integer; out AResult: Boolean);
```

### Visibility Ordering in Classes

```delphi
type
  TMyClass = class(TObject)
  private
    FField: string;
    procedure PrivateHelper;
  protected
    procedure ProtectedMethod; virtual;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;
    procedure PublicMethod;
    property Name: string read FName write FName;
  end;
```

---

## 6. DFM/FMX Rules

Special characters in string properties **must** use `#decimal` notation:

```
// Correct
Caption = 'Stra'#223'e'      // ß = U+00DF = #223
Caption = 'M'#252'nchen'     // ü = U+00FC = #252
Caption = 'Gr'#252#223'e'    // üß

// Wrong (raw UTF-8 in DFM causes corruption)
Caption = 'Straße'
```

DFM files are binary-compatible text. Never embed raw UTF-8 multi-byte sequences.

---

## 7. XML Documentation Comments

Document all public and protected members with XML Doc triple-slash comments:

```delphi
/// <summary>
///   Creates a new customer with the given name.
/// </summary>
/// <param name="AName">The customer's display name. Must not be empty.</param>
/// <returns>A new TCustomer instance. Caller owns the object.</returns>
/// <exception cref="EArgumentException">Raised when AName is empty.</exception>
constructor TCustomer.Create(const AName: string);
```

Comment the **why**, not the **what**. If the code clearly shows what happens, a comment
restating it adds no value.

---

## 8. DPROJ Rules

- Never commit DPROJ files without running `dproj_normalize.py` first
- Never add absolute paths to `DCC_UnitSearchPath` in the shared DPROJ
- Machine-specific settings belong in `.dproj.user` (gitignored)
- Always keep `VerInfo_Keys` consistent with `VerInfo_MajorVer/MinorVer/Release/Build`
- Output paths must follow `$(Platform)\$(Config)` pattern

---

## 9. Testing Conventions (DUnitX)

### Test Naming

Test method names follow the pattern: `Test_<Method>_<Condition>_<ExpectedResult>`

```delphi
[Test]
procedure Test_Calculate_WhenInputIsZero_ReturnsZero;

[Test]
procedure Test_Save_WhenCustomerIsValid_DoesNotRaiseException;

[Test]
[ExpectedException(EArgumentException)]
procedure Test_Create_WhenNameIsEmpty_RaisesArgumentException;
```

### Test Structure (AAA Pattern)

```delphi
procedure TTestCustomer.Test_UpdateName_WhenNewNameGiven_ChangesName;
var
  LCustomer: TCustomer;
begin
  // Arrange
  LCustomer := TCustomer.Create('Old Name');
  try
    // Act
    LCustomer.UpdateName('New Name');

    // Assert
    Assert.AreEqual('New Name', LCustomer.Name);
  finally
    FreeAndNil(LCustomer);
  end;
end;
```

### Test Project Structure

```
tests/
├── TestProjectName.dpr        # Console runner
├── TestProjectName.dproj      # Test project file
├── TestProjectName.groupproj  # (optional)
└── Test.*.pas                 # Test units (one per unit under test)
```

---

## 10. Build Instructions

Build using the PowerShell script in `build/`:

```powershell
# Debug build (Win32)
.\build\DelphiBuildDPROJ.ps1 -ProjectFile .\ProjectName.dproj -Config Debug -Platform Win32

# Release build (Win64)
.\build\DelphiBuildDPROJ.ps1 -ProjectFile .\ProjectName.dproj -Config Release -Platform Win64

# Both platforms
.\build\DelphiBuildDPROJ.ps1 -ProjectFile .\ProjectName.dproj -Config Release
```

Output lands in `$(Platform)\$(Config)\` (e.g., `Win32\Debug\ProjectName.exe`).
