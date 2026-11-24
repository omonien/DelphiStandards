# delphi.md Template

This file contains the template for the `delphi.md` that the skill generates in each project.
Replace `{{PLACEHOLDER}}` values when generating.

**Substitutions:**
- `{{PROJECT_NAME}}` — Project name
- `{{AUTHOR}}` — From `delphi-project.json` `header.author`, default: the user's name (`git config user.name`)
- `{{LICENSE}}` — From `delphi-project.json` `header.license`, default `MIT`
- `{{YEAR}}` — Current year
- `{{SOURCE_DIR}}` — From `delphi-project.json` `paths.sourceDir`, default `src`
- `{{TEST_DIR}}` — From `delphi-project.json` `paths.testDir`, default `tests`
- `{{INDENT}}` — From `delphi-project.json` `formatting.indentSize`, default `2`
- `{{LINE_LEN}}` — From `delphi-project.json` `formatting.maxLineLength`, default `120`
- `{{LOCAL_VAR}}` — From `delphi-project.json` `naming.localVarPrefix`, default `L`
- `{{FIELD}}` — From `delphi-project.json` `naming.fieldPrefix`, default `F`
- `{{PARAM}}` — From `delphi-project.json` `naming.paramPrefix`, default `A`

---

## Generated delphi.md Content

````markdown
# Delphi Coding Standards for {{PROJECT_NAME}}

This file contains all Delphi-specific rules for this project.
Always apply these rules when creating, editing, or reviewing Delphi source files.

---

## File Encoding (Non-Negotiable)

Always save `.pas`, `.dpr`, `.dpk` files as **UTF-8 with BOM** (byte sequence EF BB BF at start).
In Python: `open(path, 'w', encoding='utf-8-sig', newline='\r\n')`.
In any other tool: ensure the file starts with the 3-byte UTF-8 BOM.

Never save Delphi source files as UTF-8 without BOM, Latin-1, or any other encoding.

Always use **CRLF line endings** (`\r\n`) for all Delphi source files:
- `.pas`, `.dpr`, `.dpk`, `.dfm`, `.fmx`, `.dproj`

LF-only line endings cause silent failures in the Delphi IDE form designer and compiler.

Verify with: `python scripts/encoding_check.py {{SOURCE_DIR}}/ --fix`

---

## Unit Header

Every `.pas` file must begin with this XML Doc header (immediately after the BOM, before the
`unit` declaration):

```pascal
/// <summary>
///   Brief one-line description of this unit.
/// </summary>
/// <remarks>
///   Additional details, design decisions, usage notes.
/// </remarks>
/// <author>{{AUTHOR}}</author>
/// <created>YYYY-MM-DD</created>
/// <modified>YYYY-MM-DD</modified>
/// <license>{{LICENSE}}</license>
unit Unit.Name;
```

---

## Naming Conventions

### Type Prefixes

| Element | Prefix | Example |
|---------|--------|---------|
| Class | `T` | `TCustomer`, `TOrderManager` |
| Interface | `I` | `ICustomer`, `IRepository` |
| Record | `T` | `TPoint3D` |
| Exception | `E` | `ECustomerNotFound` |
| Enumeration | `T` | `TOrderStatus` |
| Enum member | none, `{$SCOPEDENUMS ON}` | `TOrderStatus.New` |

### Variable Prefixes

| Scope | Prefix | Example |
|-------|--------|---------|
| Method parameter | `{{PARAM}}` | `{{PARAM}}CustomerName: string` |
| Local variable | `{{LOCAL_VAR}}` | `{{LOCAL_VAR}}Customer: TCustomer` |
| Class field | `{{FIELD}}` | `{{FIELD}}CustomerList: TList<TCustomer>` |
| Global variable | `G` | Avoid; use `G` prefix if unavoidable |
| Loop counter | none | `i`, `j`, `k` |

### Unit Names

Use dot-notation: `Customer.Repository.pas`, `Main.Form.pas`, `Order.Details.Form.pas`
Forms end in `.Form.pas`. Data modules end in `.DM.pas`.

### Component Names

PascalCase type prefix + descriptive name. No `{{FIELD}}`/`{{LOCAL_VAR}}` prefixes:
`ButtonLogin`, `EditUserName`, `GridCustomers`, `LabelStatus`

---

## Code Formatting

- **Indentation:** {{INDENT}} spaces. No tabs.
- **Line length:** {{LINE_LEN}} characters maximum.
- **begin/end:**
  - Procedures/functions: `begin` on same line as declaration
  - `if`/`for`/`while`/`try`: `begin` on its own line
- **Parameters:** Use `const` for unchanged string/interface/record params.
  Use `var`/`out` explicitly for output params.

---

## DFM/FMX Special Characters

Special characters in form files must use `#decimal` notation, never raw UTF-8:

```
Caption = 'Stra'#223'e'      // ß
Caption = 'M'#252'nchen'     // ü
Caption = #214'sterreich'    // Ö
```

---

## Output Paths

All build output must go to `$(Platform)\$(Config)` relative paths:
- Executables: `.\$(Platform)\$(Config)`
- DCU files: `.\$(Platform)\$(Config)\dcu`

Never use absolute paths or hard-coded directory names like `bin\` or `output\`.

---

## DPROJ Rules

- Never add absolute paths to `DCC_UnitSearchPath` in the shared `.dproj`
- Machine-specific paths (local library installs, debugger settings) belong in `.dproj.user`
- Always run `python scripts/dproj_normalize.py ProjectName.dproj --in-place` before committing
- Keep `VerInfo_Keys` consistent with `VerInfo_MajorVer/MinorVer/Release/Build`
- The git pre-commit hook runs normalization automatically if installed

---

## Testing

Test framework: **DUnitX** (located in `{{TEST_DIR}}/`)
Test method naming: `Test_<Method>_<Condition>_<ExpectedResult>`
Test structure: Arrange / Act / Assert pattern

```pascal
procedure TTestCustomer.Test_UpdateName_WhenNewNameGiven_ChangesName;
var
  {{LOCAL_VAR}}Customer: TCustomer;
begin
  // Arrange
  {{LOCAL_VAR}}Customer := TCustomer.Create('Old Name');
  try
    // Act
    {{LOCAL_VAR}}Customer.UpdateName('New Name');
    // Assert
    Assert.AreEqual('New Name', {{LOCAL_VAR}}Customer.Name);
  finally
    FreeAndNil({{LOCAL_VAR}}Customer);
  end;
end;
```

---

## Build Instructions

```powershell
# Debug build
.\build\DelphiBuildDPROJ.ps1 -ProjectFile .\{{PROJECT_NAME}}.dproj -Config Debug -Platform Win32

# Release build
.\build\DelphiBuildDPROJ.ps1 -ProjectFile .\{{PROJECT_NAME}}.dproj -Config Release -Platform Win64
```

Output: `$(Platform)\$(Config)\{{PROJECT_NAME}}.exe`

---

## Project Structure

```
{{SOURCE_DIR}}/         Source code
{{TEST_DIR}}/           DUnitX test project
libs/                   Git submodules (DUnitX, etc.)
build/                  Build scripts and output
docs/                   Documentation
res/                    Resources
```

---

## Copyright

All new units: `Copyright © {{YEAR}} {{AUTHOR}}` / `{{LICENSE}}`
````
