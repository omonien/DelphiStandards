# Project Templates

DPROJ templates for all supported project types. Replace all `{{PLACEHOLDER}}` markers before use.

**Required substitutions:**
- `{{GUID}}` — Fresh GUID, uppercase with braces: `{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}`
- `{{PROJECT_NAME}}` — Project name without extension: `MyApplication`
- `{{PROJECT_VERSION}}` — Delphi target: `19.5` (D11), `20.2` (D12), `20.4` (D13.1), `20.5` (D13.2)
- `{{YEAR}}` — Current year: `2026`

Generate a GUID: `python -c "import uuid; print('{'+str(uuid.uuid4()).upper()+'}')" `

---

## VCL Application

**`.dpr` file:**

```pascal
program {{PROJECT_NAME}};

uses
  Vcl.Forms,
  Main.Form in 'src\Main.Form.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
```

**`.dproj` file:**

```xml
<?xml version="1.0" encoding="utf-8"?>
<Project DefaultTargets="Build" ToolsVersion="4.0" xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
  <PropertyGroup>
    <ProjectGuid>{{GUID}}</ProjectGuid>
    <ProjectVersion>{{PROJECT_VERSION}}</ProjectVersion>
    <FrameworkType>VCL</FrameworkType>
    <MainSource>{{PROJECT_NAME}}.dpr</MainSource>
    <Base>true</Base>
    <Config Condition="'$(Config)'==''">Debug</Config>
    <Platform Condition="'$(Platform)'==''">Win32</Platform>
    <TargetedPlatforms>3</TargetedPlatforms>
    <AppType>Application</AppType>
    <SanitizedProjectName>{{PROJECT_NAME}}</SanitizedProjectName>
    <IncludeVerInfo>True</IncludeVerInfo>
    <VerInfo_MajorVer>1</VerInfo_MajorVer>
    <VerInfo_MinorVer>0</VerInfo_MinorVer>
    <VerInfo_Release>0</VerInfo_Release>
    <VerInfo_Build>0</VerInfo_Build>
    <VerInfo_AutoIncBuild>False</VerInfo_AutoIncBuild>
    <VerInfo_Keys>CompanyName=;FileDescription={{PROJECT_NAME}};FileVersion=1.0.0.0;InternalName={{PROJECT_NAME}};LegalCopyright=Copyright © {{YEAR}};LegalTrademarks=;OriginalFilename={{PROJECT_NAME}}.exe;ProductName={{PROJECT_NAME}};ProductVersion=1.0.0.0;Comments=</VerInfo_Keys>
  </PropertyGroup>
  <PropertyGroup Condition="'$(Base)'!=''">
    <DCC_DcuOutput>.\$(Platform)\$(Config)\dcu</DCC_DcuOutput>
    <DCC_ExeOutput>.\$(Platform)\$(Config)</DCC_ExeOutput>
    <DCC_Namespace>System;Xml;Data;Datasnap;Web;Soap;Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell</DCC_Namespace>
    <DCC_UnitSearchPath>.\src</DCC_UnitSearchPath>
  </PropertyGroup>
  <PropertyGroup Condition="'$(Cfg_1)'!=''">
    <DCC_Define>DEBUG;$(DCC_Define)</DCC_Define>
    <DCC_Optimize>false</DCC_Optimize>
    <DCC_DebugInfoInExe>true</DCC_DebugInfoInExe>
  </PropertyGroup>
  <PropertyGroup Condition="'$(Cfg_2)'!=''">
    <DCC_Define>RELEASE;$(DCC_Define)</DCC_Define>
    <DCC_Optimize>true</DCC_Optimize>
    <DCC_DebugInfoInExe>false</DCC_DebugInfoInExe>
    <DCC_LocalDebugSymbols>false</DCC_LocalDebugSymbols>
    <DCC_SymbolReferenceInfo>0</DCC_SymbolReferenceInfo>
  </PropertyGroup>
  <ItemGroup>
    <BuildConfiguration Include="Base">
      <Key>Base</Key>
    </BuildConfiguration>
    <BuildConfiguration Include="Debug">
      <Key>Cfg_1</Key>
      <CfgParent>Base</CfgParent>
    </BuildConfiguration>
    <BuildConfiguration Include="Release">
      <Key>Cfg_2</Key>
      <CfgParent>Base</CfgParent>
    </BuildConfiguration>
  </ItemGroup>
  <ItemGroup>
    <DelphiCompile Include="$(MainSource)">
      <MainSource>MainSource</MainSource>
    </DelphiCompile>
    <DCCReference Include="src\Main.Form.pas">
      <Form>TFormMain</Form>
      <FormType>dfm</FormType>
    </DCCReference>
  </ItemGroup>
  <ProjectExtensions>
    <Borland.Personality>Delphi.Personality.12</Borland.Personality>
    <Borland.ProjectType>Application</Borland.ProjectType>
    <BorlandProject>
      <Delphi.Personality>
        <Source>
          <Source Name="MainSource">{{PROJECT_NAME}}.dpr</Source>
        </Source>
      </Delphi.Personality>
      <Platforms>
        <Platform value="Win32">True</Platform>
        <Platform value="Win64">True</Platform>
      </Platforms>
    </BorlandProject>
    <ProjectFileVersion>12</ProjectFileVersion>
  </ProjectExtensions>
  <Import Project="$(BDS)\Bin\CodeGear.Delphi.Targets"
          Condition="Exists('$(BDS)\Bin\CodeGear.Delphi.Targets')"/>
</Project>
```

---

## FMX Application

**`.dpr` file:**

```pascal
program {{PROJECT_NAME}};

uses
  System.StartUpCopy,
  FMX.Forms,
  Main.Form in 'src\Main.Form.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
```

**`.dproj` changes from VCL template:**
- `FrameworkType`: `FMX`
- `DCC_Namespace`: `System;Xml;Data;Datasnap;Web;Soap;FMX;FMX.Surfaces;FMX.Import`
- `TargetedPlatforms`: `3` for Windows-only, higher values for cross-platform
- `FormType` in DCCReference: `fmx`

---

## Console Application

**`.dpr` file:**

```pascal
program {{PROJECT_NAME}};

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils;

begin
  try
    Writeln('{{PROJECT_NAME}}');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
```

**`.dproj` changes from VCL template:**
- `FrameworkType`: `None`
- `AppType`: `Console`
- `DCC_Namespace`: `System;Xml;Data;Datasnap;Web;Soap`
- No `DCCReference` entries initially (add units as needed)
- `TargetedPlatforms`: `3` (Win32+Win64) or `131` (Win32+Win64+Linux64)

---

## Package (BPL)

**`.dpk` file:**

```pascal
package {{PROJECT_NAME}};

{$R *.res}
{$IFDEF IMPLICITBUILDING This IFDEF should not be used by users}
{$ALIGN 8}
{$ASSERTIONS ON}
{$BOOLEVAL OFF}
{$DEBUGINFO OFF}
{$EXTENDEDSYNTAX ON}
{$IMPORTEDDATA ON}
{$IOCHECKS ON}
{$LOCALSYMBOLS ON}
{$LONGSTRINGS ON}
{$OPENSTRINGS ON}
{$OPTIMIZATION OFF}
{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}
{$REFERENCEINFO ON}
{$SAFEDIVIDE OFF}
{$STACKFRAMES ON}
{$TYPEDADDRESS OFF}
{$VARSTRINGCHECKS ON}
{$WRITEABLECONST OFF}
{$MINENUMSIZE 1}
{$IMAGEBASE $400000}
{$DEFINE DEBUG}
{$ENDIF IMPLICITBUILDING}
{$IMPLICITBUILD ON}

requires
  rtl,
  vcl;

contains
  My.Unit in 'src\My.Unit.pas';

end.
```

**`.dproj` changes from VCL template:**
- `FrameworkType`: `VCL` (or `None` for RTL-only packages)
- `AppType`: `Package`
- `DCC_BplOutput`: `.\$(Platform)\$(Config)`
- `DCC_DcpOutput`: `.\$(Platform)\$(Config)\dcu`
- Add `<VerInfo_Keys>` with `OriginalFilename={{PROJECT_NAME}}.bpl`
- `Borland.ProjectType`: `Package`

---

## DUnitX Test Project

**`.dpr` file:**

```pascal
program Test{{PROJECT_NAME}};

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  Test.{{PROJECT_NAME}}.SomeUnit in 'Test.{{PROJECT_NAME}}.SomeUnit.pas';

var
  Runner: ITestRunner;
  Results: IRunResults;
  Logger: ITestLogger;
  NUnitLogger: ITestLogger;
begin
  try
    TDUnitX.CheckCommandLine;
    Runner := TDUnitX.CreateRunner;
    Runner.UseRTTI := True;

    Logger := TDUnitXConsoleLogger.Create(True);
    Runner.AddLogger(Logger);

    NUnitLogger := TDUnitXXmlNUnitFileLogger.Create(
      TDUnitX.Options.XMLOutputFile
    );
    Runner.AddLogger(NUnitLogger);

    Runner.FailsOnNoAsserts := False;

    Results := Runner.Execute;
    if not Results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    {$IFNDEF CI}
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
    {$ENDIF}
  except
    on E: Exception do
    begin
      System.Writeln(E.ClassName, ': ', E.Message);
      System.ExitCode := EXIT_ERRORS;
    end;
  end;
end.
```

**Sample test unit `Test.{{PROJECT_NAME}}.SomeUnit.pas`:**

```pascal
unit Test.{{PROJECT_NAME}}.SomeUnit;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TTestSomeUnit = class
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure Test_SomeMethod_WhenCondition_ReturnsExpectedResult;
  end;

implementation

procedure TTestSomeUnit.Setup;
begin
  // Initialize test fixture
end;

procedure TTestSomeUnit.TearDown;
begin
  // Clean up test fixture
end;

procedure TTestSomeUnit.Test_SomeMethod_WhenCondition_ReturnsExpectedResult;
begin
  // Arrange

  // Act

  // Assert
  Assert.IsTrue(True, 'Replace with actual assertion');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestSomeUnit);

end.
```

**`.dproj` for test project:**
- `FrameworkType`: `None`
- `AppType`: `Console`
- `DCC_UnitSearchPath`: `..\src;..\libs\DUnitX\Source`
- No form references

---

## DLL Library

**`.dpr` file:**

```pascal
library {{PROJECT_NAME}};

{$R *.res}

uses
  System.SysUtils,
  System.Classes;

exports
  // Export your functions here

begin
end.
```

**`.dproj` changes from Console template:**
- `AppType`: `Library`
- `OriginalFilename` in `VerInfo_Keys`: `{{PROJECT_NAME}}.dll`

---

## Project Group (.groupproj)

```xml
<?xml version="1.0" encoding="utf-8"?>
<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
  <ProjectGroup>
    <Projects Include="{{PROJECT_NAME}}.dproj">
      <Dependencies/>
    </Projects>
    <Projects Include="tests\Test{{PROJECT_NAME}}.dproj">
      <Dependencies/>
    </Projects>
  </ProjectGroup>
  <ProjectExtensions>
    <Borland.Personality>Default.Personality.12</Borland.Personality>
  </ProjectExtensions>
  <Import Project="$(BDS)\Bin\CodeGear.Group.Targets"
          Condition="Exists('$(BDS)\Bin\CodeGear.Group.Targets')"/>
</Project>
```
