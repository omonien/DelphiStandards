# DPROJ File Format Reference

The `.dproj` file is an MSBuild 2003 XML project file with Delphi-specific extensions.
Root element: `<Project DefaultTargets="Build" ToolsVersion="4.0" xmlns="http://schemas.microsoft.com/developer/msbuild/2003">`

No official XSD schema is published by Embarcadero. The authoritative property/task definitions
reside in `$(BDS)\Bin\CodeGear.Delphi.Targets`.

---

## Canonical Section Order

A well-formed DPROJ must have sections in this order:

```
<Project>
  1. PropertyGroup (base — no Condition, or Condition="'$(Base)'!=''")
  2. PropertyGroup (Base_Win32, Base_Win64, etc. — platform-specific base settings)
  3. PropertyGroup (Cfg_1 = Debug, Cfg_2 = Release)
  4. PropertyGroup (Cfg_1_Win32, Cfg_1_Win64, Cfg_2_Win32, etc. — platform+config combos)
  5. ItemGroup (BuildConfiguration definitions)
  6. ItemGroup (DelphiCompile + DCCReference entries)
  7. ItemGroup (Deployment entries — mobile/cross-platform targets)
  8. ItemDefinitionGroup (build events — optional)
  9. ProjectExtensions (Borland metadata)
  10. Import (CodeGear.Delphi.Targets)
</Project>
```

---

## PropertyGroup: Base (No Condition)

Contains global project identity settings. These never change per-platform or per-config.

| Element | Purpose | Example |
|---------|---------|---------|
| `ProjectGuid` | Unique project identifier | `{93B8A149-40C5-44F2-AF82-FD06AA5CEC4A}` |
| `ProjectVersion` | Delphi version tag | `20.2` |
| `FrameworkType` | UI framework | `VCL`, `FMX`, `None` |
| `MainSource` | Name of the `.dpr` file | `MyProject.dpr` |
| `Base` | Always `true` | `true` |
| `Config` | Default configuration | `Debug` (with `Condition="'$(Config)'==''"`) |
| `Platform` | Default platform | `Win32` (with `Condition="'$(Platform)'==''"`) |
| `TargetedPlatforms` | Platform bitmask | `3` |
| `AppType` | Application type | `Application`, `Console`, `Package`, `Library` |
| `SanitizedProjectName` | Project name without special chars | `MyProject` |

### TargetedPlatforms Bitmask

Values derived from IDE-written project files (XE3 .. 13.2):

| Platform | Bit | Decimal |
|----------|-----|---------|
| Win32 | bit 0 | 1 |
| Win64 | bit 1 | 2 |
| OSX32 (legacy) | bit 2 | 4 |
| iOSSimulator (legacy) | bit 3 | 8 |
| Android (32-bit) | bit 4 | 16 |
| iOSDevice32 (legacy) | bit 6 | 64 |
| Linux64 | bit 7 | 128 |
| iOSDevice64 | bit 10 | 1024 |
| OSX64 | bit 12 | 4096 |
| Android64 | bit 15 | 32768 |
| OSXARM64 | bit 17 | 131072 |
| iOSSimARM64 | bit 19 | 524288 |
| Win64x (C++ Modern) | bit 20 | 1048576 |
| WinARM64EC (13.1+) | bit 21 | 2097152 |

Common values: `1` = Win32 only, `3` = Win32+Win64, `131` = Win32+Win64+Linux64,
`32784` = Android+Android64, `32787` = Win32+Win64+Android+Android64,
`693267` = Win32+Win64+Android+Android64+OSX64+OSXARM64+iOSDevice64+iOSSimARM64.

### ProjectVersion by Delphi Version

| Delphi | ProjectVersion |
|--------|---------------|
| 10.3 Rio | `18.5` – `18.8` |
| 10.4 Sydney | `19.0` – `19.2` |
| 11 Alexandria | `19.3` – `19.5` |
| 12 Athens | `20.1` (12.0/12.1), `20.2` (12.2), `20.3` (12.3) |
| 13 Florence | `20.3` (13.0), `20.4` (13.1), `20.5` (13.2) |

**Note:** Delphi 12.3 and 13.0 share `20.3` — indistinguishable by ProjectVersion alone.
**Default for new projects:** Delphi 13 Florence → `20.4` (13.1; a 13.2 IDE upgrades it to `20.5` on save).
Source: [Delphi-Version-Information](https://github.com/omonien/Delphi-Version-Information).

---

## PropertyGroup: Base Platform-Specific

Condition pattern: `('$(Platform)'=='Win32' and '$(Base)'=='true') or '$(Base_Win32)'!=''`

Contains per-platform base settings:

| Element | Purpose | Example |
|---------|---------|---------|
| `DCC_DcuOutput` | DCU output path | `.\$(Platform)\$(Config)\dcu` |
| `DCC_ExeOutput` | Executable output path | `.\$(Platform)\$(Config)` |
| `DCC_BplOutput` | Package .bpl output | `.\$(Platform)\$(Config)` |
| `DCC_DcpOutput` | Package .dcp output | `.\$(Platform)\$(Config)\dcu` |
| `DCC_ObjOutput` | Object file output | `.\$(Platform)\$(Config)\dcu` |
| `DCC_Namespace` | Default namespace prefixes | see below |
| `DCC_UnitSearchPath` | Semicolon-delimited search paths | `.\src;..\libs\DUnitX\Source` |

**DCC_Namespace by framework:**
- VCL: `System;Xml;Data;Datasnap;Web;Soap;Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell`
- FMX: `System;Xml;Data;Datasnap;Web;Soap;FMX;FMX.Surfaces;FMX.Import`
- Console/None: `System;Xml;Data;Datasnap;Web;Soap`

---

## PropertyGroup: Debug Configuration (Cfg_1)

Condition: `'$(Cfg_1)'!=''`

| Element | Value |
|---------|-------|
| `DCC_Define` | `DEBUG;$(DCC_Define)` |
| `DCC_Optimize` | `false` |
| `DCC_DebugInfoInExe` | `true` |
| `DCC_LocalDebugSymbols` | `true` (implied default) |

---

## PropertyGroup: Release Configuration (Cfg_2)

Condition: `'$(Cfg_2)'!=''`

| Element | Value |
|---------|-------|
| `DCC_Define` | `RELEASE;$(DCC_Define)` |
| `DCC_Optimize` | `true` |
| `DCC_DebugInfoInExe` | `false` |
| `DCC_LocalDebugSymbols` | `false` |
| `DCC_SymbolReferenceInfo` | `0` |

---

## Complete DCC_ Property Reference

### Output Paths
| Property | Purpose |
|----------|---------|
| `DCC_DcuOutput` | Compiled unit (.dcu) directory |
| `DCC_ExeOutput` | Executable directory |
| `DCC_ObjOutput` | Object file directory |
| `DCC_BplOutput` | Package (.bpl) directory |
| `DCC_DcpOutput` | Package interface (.dcp) directory |

### Search Paths
| Property | Purpose |
|----------|---------|
| `DCC_UnitSearchPath` | Pascal unit search directories |
| `DCC_ResourcePath` | Resource file directories |
| `DCC_ObjPath` | Object file search directories |
| `DCC_IncludePath` | Include file directories |

### Compiler Switches
| Property | Purpose |
|----------|---------|
| `DCC_Define` | Conditional defines (semicolon-separated) |
| `DCC_Optimize` | Enable optimization (`true`/`false`) |
| `DCC_DebugInfoInExe` | Embed debug info in EXE (`true`/`false`) |
| `DCC_LocalDebugSymbols` | Local debug symbols (`true`/`false`) |
| `DCC_SymbolReferenceInfo` | Symbol reference info level (`0`/`1`/`2`) |
| `DCC_MapFile` | Generate map file (`0`=none, `3`=detailed) |
| `DCC_Align` | Record field alignment |
| `DCC_IOChecks` | I/O checking |
| `DCC_RangeChecking` | Range checking |
| `DCC_OverflowChecking` | Overflow checking |
| `DCC_AssertionLevel` | Assertion level |
| `DCC_BoolEvalMode` | Complete boolean evaluation |
| `DCC_Namespace` | Default namespace prefixes |
| `DCC_UsePackage` | Runtime packages list |
| `DCC_BuildAllUnits` | Force rebuild all units |
| `DCC_Quiet` | Suppress compiler output |
| `DCC_RemoteDebug` | Remote debugging |
| `DCC_AdditionalSwitches` | Raw additional compiler switches |

### Version Information
| Property | Purpose |
|----------|---------|
| `IncludeVerInfo` | Embed version info (`True`/`False`) |
| `VerInfo_MajorVer` | Major version number |
| `VerInfo_MinorVer` | Minor version number |
| `VerInfo_Release` | Release/patch number |
| `VerInfo_Build` | Build number |
| `VerInfo_AutoIncBuild` | Auto-increment build (`False` recommended) |
| `VerInfo_Keys` | Semicolon-delimited key=value pairs |

**VerInfo_Keys standard key order:**
```
CompanyName=;FileDescription=;FileVersion=1.0.0.0;InternalName=;
LegalCopyright=;LegalTrademarks=;OriginalFilename=;
ProductName=;ProductVersion=1.0.0.0;Comments=
```

---

## ItemGroup: BuildConfiguration

Always present. Defines the configuration inheritance chain.

```xml
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
```

Custom configurations (e.g., Staging) use `Cfg_3`, `Cfg_4`, etc.

---

## ItemGroup: Source Files

```xml
<ItemGroup>
  <!-- Main .dpr file -->
  <DelphiCompile Include="$(MainSource)">
    <MainSource>MainSource</MainSource>
  </DelphiCompile>

  <!-- Plain unit (no form) -->
  <DCCReference Include="My.Unit.pas"/>

  <!-- Unit with VCL/FMX form -->
  <DCCReference Include="Main.Form.pas">
    <Form>TFormMain</Form>
    <FormType>dfm</FormType>      <!-- dfm for VCL, fmx for FMX -->
  </DCCReference>

  <!-- Data module -->
  <DCCReference Include="Customer.DM.pas">
    <Form>TDMCustomer</Form>
    <DesignClass>TDataModule</DesignClass>
  </DCCReference>

  <!-- Frame -->
  <DCCReference Include="Header.Frame.pas">
    <Form>TFrameHeader</Form>
    <DesignClass>TFrame</DesignClass>
  </DCCReference>
</ItemGroup>
```

---

## ItemDefinitionGroup: Build Events

Optional. Contains pre/post-build commands.

```xml
<ItemDefinitionGroup>
  <PreBuildEvent>
    <Command>echo Pre-build step</Command>
    <Message>Running pre-build</Message>
  </PreBuildEvent>
  <PostBuildEvent>
    <Command>echo Post-build step</Command>
  </PostBuildEvent>
</ItemDefinitionGroup>
```

Related properties in base PropertyGroup:
- `PreBuildEventUseInBuild`, `PreLinkEventUseInBuild`, `PostBuildEventUseInBuild`

---

## ProjectExtensions

Borland/Embarcadero metadata block. Not processed by MSBuild directly.

```xml
<ProjectExtensions>
  <Borland.Personality>Delphi.Personality.12</Borland.Personality>
  <Borland.ProjectType>Application</Borland.ProjectType>
  <BorlandProject>
    <Delphi.Personality>
      <Excluded_Packages/>
      <Source>
        <Source Name="MainSource">ProjectName.dpr</Source>
      </Source>
    </Delphi.Personality>
    <Platforms>
      <Platform value="Win32">True</Platform>
      <Platform value="Win64">True</Platform>
    </Platforms>
  </BorlandProject>
  <ProjectFileVersion>12</ProjectFileVersion>
</ProjectExtensions>
```

The `<Platforms>` block should be consistent with `TargetedPlatforms` in the base PropertyGroup. In practice
Delphi 13 projects were seen where they disagree (e.g. `TargetedPlatforms` = `1048576` while Android64,
Win64, iOS and macOS are enabled in `<Platforms>`). Treat the `<Platforms>` list as authoritative for which
platforms are in use; never delete a platform that is enabled there.

---

## Import Element

Must be the last element in the file.

```xml
<Import Project="$(BDS)\Bin\CodeGear.Delphi.Targets"
        Condition="Exists('$(BDS)\Bin\CodeGear.Delphi.Targets')"/>
```

`$(BDS)` resolves to the Delphi IDE installation directory (set by the IDE or RAD Studio command prompt).

---

## Condition Attribute Patterns

| Pattern | When Active |
|---------|------------|
| (no Condition) | Always (base PropertyGroup) |
| `'$(Base)'!=''` | Base configuration only |
| `'$(Cfg_1)'!=''` | Debug configuration |
| `'$(Cfg_2)'!=''` | Release configuration |
| `'$(Base_Win32)'!=''` | Win32 base settings |
| `'$(Base_Win64)'!=''` | Win64 base settings |
| `'$(Cfg_1_Win32)'!=''` | Win32 Debug settings |
| `'$(Cfg_2_Win64)'!=''` | Win64 Release settings |
| `('$(Platform)'=='Win32' and '$(Base)'=='true') or '$(Base_Win32)'!=''` | Win32 platform (long form) |

---

## Compatibility Notes

- **Forward incompatible:** Older Delphi cannot open newer DPROJ files.
- **Backward compatible:** Newer Delphi auto-converts older format (one-way upgrade).
- `FrameworkType` added in XE2.
- Multi-platform target support added in XE5.
- Random element ordering within PropertyGroups: started in Delphi 10 Seattle (dictionary-based
  internal storage). Use `dproj_normalize.py` to restore deterministic ordering.
