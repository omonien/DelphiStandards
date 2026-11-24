<div align="left">
  <a href="https://www.embarcadero.com/products/delphi">
    <img src="https://commons.wikimedia.org/wiki/Special:FilePath/Delphi_Logo_12.svg" alt="Delphi Logo" width="120" />
  </a>
</div>

# Delphi Standards

[![Lang-DE](https://img.shields.io/badge/lang-DE-blue.svg)](README.de.md) [![Lang-EN](https://img.shields.io/badge/lang-EN-lightgrey.svg)](README.md)

> Beiträge in Deutsch oder Englisch sind willkommen. Die Sprachsynchronisierung erfolgt durch den Maintainer spätestens nach dem Merge.

## Übersicht

Dieses Projekt stellt eine kompakte, praxiserprobte Grundlage für einheitlichen Delphi‑Code bereit. Im Fokus stehen:

- Ein klarer, moderner Delphi Style Guide (Deutsch und Englisch)
- Eine kuratierte .gitignore‑Konfiguration speziell für Delphi
- Eine .gitattributes‑Vorlage zur EOL‑Normalisierung und zum Schutz von Binärdateien
- Agent-Skills, die KI-Coding-Agenten (Claude Code, Cursor, grok CLI, Codex) diese Standards beibringen

Beides hilft Teams, konsistenten Code zu schreiben und typische Reibungen zu vermeiden.

## Warum das wichtig ist

Gerade in Team‑Repos werden in der Eile häufig Dateien eingecheckt, die nicht in die Versionsverwaltung gehören (IDE‑Artefakte, Build‑Outputs, lokale Einstellungen). Das führt zu:

- unnötigen Diffs und Merge‑Konflikten
- instabilen Builds durch lokale Artefakte
- unerwünschten Seiteneffekten im Team

Die enthaltene, auf Delphi zugeschnittene .gitignore‑Datei verhindert genau das. Der Style Guide sorgt parallel für ein gemeinsames Verständnis von Struktur, Benennung und modernen Sprachfeatures.
Zusätzlich verhindert eine .gitattributes‑Datei Scheindiffs durch unterschiedliche Zeilenenden (CRLF vs. LF) und schützt Binärartefakte vor Textfiltern/Diffs. Details finden sich im Abschnitt Inhalte.

Auch außerhalb von Teams profitieren Einzelentwickler langfristig von einem konsistenten Style Guide. Dieser Leitfaden orientiert sich an bewährten, klassischen Strukturen, wie man sie auch im RTL/VCL/FMX‑Code von Delphi findet – ist aber sicherlich meinungsstark formuliert. Entscheidend ist nicht der „perfekte“ Stil, sondern dass man sich auf einen Stil einigt und ihn konsequent im gesamten Codebestand anwendet.

## Inhalte

- Kuratierte GitIgnore‑Vorlage: [Delphi GitIgnore.txt](Delphi%20GitIgnore.txt)
- Git Attributes‑Vorlage: [Delphi GitAttributes.txt](Delphi%20GitAttributes.txt)
- Delphi Style Guide (DE): [Delphi Style Guide DE.md](Delphi%20Style%20Guide%20DE.md)
- Delphi Style Guide (EN): [Delphi Style Guide EN.md](Delphi%20Style%20Guide%20EN.md)
- Universelles Build-Skript: [DelphiBuildDPROJ.ps1](DelphiBuildDPROJ.ps1) - PowerShell-Skript zum Bauen von Delphi-Projekten per Kommandozeile
- Release Tools: [release-tools/](release-tools/) - Automatisierte Release-Erstellung
- Agent-Skills: [plugins/](plugins/) - Skills für Claude Code, Cursor, grok CLI und Codex (siehe [Agent-Skills](#agent-skills-claude-code-cursor-grok-cli-codex))

Hinweis: Der Style Guide wird synchron in Deutsch und Englisch gepflegt. Änderungen sollten immer in beiden Dokumenten erfolgen.

## Schnellstart

1. Style Guide lesen und im Team vereinbaren

- Einrückung: 2 Leerzeichen; Zeilenlänge: 120 Zeichen (Formatter/Editor‑Guideline synchron halten)
- Namenskonventionen (A-/L-/F‑Präfixe, Komponenten, Konstanten, Enums mit SCOPEDENUMS)
- Moderne Features: Generics, anonyme Methoden, Inline‑Variablen (10.3+), Multiline‑Strings (12+)

1. .gitignore aktivieren

- Datei „Delphi GitIgnore.txt“ ins Repo‑Root kopieren und in „.gitignore“ umbenennen
- Projektspezifische Ergänzungen nach Bedarf vornehmen

1. Git Attributes hinzufügen

- „Delphi GitAttributes.txt“ ins Repo‑Root kopieren und in „.gitattributes“ umbenennen.
- Die Datei normalisiert Zeilenenden für Delphi‑Quellen (CRLF) und markiert Binärartefakte (.res, .ico, .dcu, .bpl, .dll, .exe) als binary.
- Werden .dfm/.fmx im Projekt als Text gespeichert (Standard), die Text‑Regeln beibehalten. Bei Binärspeicherung die Binary‑Regeln aus dem Template aktivieren.
- Für bestehende Repos nach dem Hinzufügen ausführen: `git add --renormalize .` und die Änderungen committen.

## Build-Automatisierung

### Universelles DPROJ Build-Skript

Das Repository enthält ein universelles PowerShell-Skript zum Bauen von Delphi-Projekten per Kommandozeile. Dies ist ideal für CI/CD-Pipelines, automatisierte Builds oder jedes Szenario, in dem Delphi-Projekte ohne IDE gebaut werden müssen.

**Features:**

- Erkennt automatisch die neueste installierte Delphi-Version aus der Windows-Registry
- Unterstützt alle Delphi-Projektdateien (.dproj)
- Konfigurierbare Build-Konfiguration (Debug/Release) und Plattform (Win32/Win64/Linux64)
- Linux-Debug-Symbol- und Map-Datei-Unterstützung über `-LinuxMap`
- Farbige Konsolenausgabe für bessere Lesbarkeit
- Verbose-Modus für detaillierte Build-Informationen
- Keine Abhängigkeiten von spezifischen Projekten - funktioniert mit jedem Delphi-Projekt
- Konfigurierbare Defaults-Sektion zur einfachen Anpassung
- Robuste Fehlerbehandlung und saubere Exit-Codes für CI/CD-Integration

**Verwendung:**

```powershell
# Einfache Verwendung (erkennt Delphi-Version automatisch, baut Debug/Win32)
.\DelphiBuildDPROJ.ps1 -ProjectFile "MeinProjekt.dproj"

# Release für Win64 bauen
.\DelphiBuildDPROJ.ps1 -ProjectFile "MeinProjekt.dproj" -Config Release -Platform Win64

# Spezifische Delphi-Version mit ausführlicher Ausgabe verwenden
.\DelphiBuildDPROJ.ps1 -ProjectFile "MeinProjekt.dproj" -DelphiVersion "22.0" -VerboseOutput

# Für Linux64 mit vollständigen Debug-Symbolen und Map-Datei bauen
.\DelphiBuildDPROJ.ps1 -ProjectFile "MeinProjekt.dproj" -Platform Linux64 -LinuxMap
```

**Parameter:**

- `-ProjectFile` (erforderlich): Pfad zur .dproj-Datei, die gebaut werden soll
- `-Config`: Build-Konfiguration (Standard: "Debug")
- `-Platform`: Zielplattform (Standard: "Win32")
- `-DelphiVersion`: Zu verwendende Delphi-Version (Standard: automatische Erkennung der neuesten Version)
- `-VerboseOutput`: Aktiviert ausführliche MSBuild-Ausgabe
- `-LinuxMap`: Aktiviert Linux-Debug-Symbole und Map-Datei-Generierung (setzt `DCC_MapFile=3`, `DCC_DebugInfo=3`, `DCC_LocalSymbols`, `DCC_LocalDebugSymbols`, `DCC_DebugInformation=2`, `DCC_DebugInfoInExe=false`)

**Voraussetzungen:**

- Embarcadero Delphi installiert
- MSBuild verfügbar (Visual Studio Build Tools oder vollständiges Visual Studio)

Weitere Details finden Sie in der Skript-Dokumentation in [DelphiBuildDPROJ.ps1](DelphiBuildDPROJ.ps1).

### WSL-Wrapper

Für Entwickler, die in WSL (Windows Subsystem for Linux) arbeiten, enthält das Repository einen Bash-Wrapper [DelphiBuildDPROJ.sh](DelphiBuildDPROJ.sh), der WSL über `powershell.exe` an die Windows-Delphi-Toolchain anbindet. Er akzeptiert dieselben Parameter wie das PowerShell-Skript und übersetzt WSL-Pfade automatisch in Windows-Pfade.

**Verwendung (aus WSL):**

```bash
# Skript ausführbar machen (einmalig)
chmod +x ./DelphiBuildDPROJ.sh

# Einfache Verwendung
./DelphiBuildDPROJ.sh -ProjectFile ./MeinProjekt.dproj

# Release für Win64 bauen
./DelphiBuildDPROJ.sh -ProjectFile ./MeinProjekt.dproj -Config Release -Platform Win64

# Für Linux64 mit vollständigen Debug-Symbolen und Map-Datei bauen
./DelphiBuildDPROJ.sh -ProjectFile ./MeinProjekt.dproj -Platform Linux64 -LinuxMap
```

**Voraussetzungen:**

- WSL (Windows Subsystem for Linux)
- `powershell.exe` aus WSL heraus im `PATH` erreichbar (bei Standard-WSL-Installationen gegeben)
- Embarcadero Delphi auf dem Windows-Host installiert

## Agent-Skills (Claude Code, Cursor, grok CLI, Codex)

Der Ordner [plugins/](plugins/) enthält Skills für KI-Coding-Agenten: pro Thema eine `SKILL.md` mit Skripten und Referenzen, die einem Agenten genau vorgeben, wie eine Delphi-spezifische Aufgabe zu erledigen ist. Sie bauen auf dem Style Guide und den Git-Vorlagen dieses Repositorys auf.

| Skill | Inhalt |
| --- | --- |
| [delphi-project](plugins/delphi-project/skills/delphi-project/SKILL.md) | Delphi-Projekte anlegen und pflegen: DPROJ-Validierung und -Normalisierung, Projektgerüst nach diesem Style Guide, Encoding-Prüfung (UTF-8 BOM + CRLF), DUnitX. Delphi 11–13. |
| [dx-data](plugins/dx-data/skills/dx-data/SKILL.md) | DX.Data Micro-ORM: Schema aus Entity-Attributen, Validierung, Mapping, Migrationen, Unit of Work (SQLite, PostgreSQL, MS SQL Server). |
| [delphi-android-r8](plugins/delphi-android-r8/skills/delphi-android-r8/SKILL.md) | R8 für Delphi-FMX-Android-App-Bundles: behebt die Google-Play-Warnung zu fehlender DEX-Optimierung/Obfuskierung mit einem getesteten MSBuild-Hook, Vorher-Nachher-Messung und verpflichtendem Starttest. |

**Claude Code** (dieses Repository ist ein Plugin-Marktplatz):

```text
/plugin marketplace add omonien/DelphiStandards
/plugin install delphi-project@delphi-standards
/plugin install dx-data@delphi-standards
/plugin install delphi-android-r8@delphi-standards
```

**Cursor, grok CLI, Codex und andere Agenten** lesen ihre Anweisungen aus dem Repository, in dem sie arbeiten. Dieses Repository klonen (oder als Submodul einbinden, z. B. `tools/DelphiStandards`) und in der `AGENTS.md` (oder `CLAUDE.md`) des eigenen Projekts auf die Skills verweisen:

```markdown
## Delphi skills
For Delphi project and DPROJ work follow tools/DelphiStandards/plugins/delphi-project/skills/delphi-project/SKILL.md.
For Android packaging, R8 or Google Play DEX warnings follow
tools/DelphiStandards/plugins/delphi-android-r8/skills/delphi-android-r8/SKILL.md.
Read the whole SKILL.md before acting; its scripts are in the scripts/ folder next to it.
```

Die Skripte brauchen Python 3.10+ (`winget install Python.Python.3.12`); delphi-android-r8 zusätzlich ein JDK 17+, R8 9.0.32 (lädt das mitgelieferte `fetch_r8.ps1`) und zum Testen `bundletool` und `adb`.

## Releases

Offizielle Releases sind im Verzeichnis [release-assets/](release-assets/) verfügbar. Jedes Release enthält:

- PDF-Versionen der Style Guides (Deutsch und Englisch)
- Git-Templates (.gitignore und .gitattributes)
- Dokumentation

Für die Erstellung neuer Releases siehe [release-tools/README.md](release-tools/README.md).

## Mitwirken

- Issues und Pull Requests sind willkommen
- Bitte Änderungen im Style Guide stets in DE und EN spiegeln (die Synchronisierung erfolgt spätestens nach dem Merge durch den Maintainer)

## Ideen

- Konfigurations-Templates für Code-Formatter, um den Style Guide technisch umzusetzen (z. B. 2 Leerzeichen, 120 Zeichen, synchronisierte vertikale Guideline) ...
- Gemeinsame Projekt-Presets (.dproj) oder IDE-Einstellungen fürs Team

## Lizenz

MIT License – siehe Hinweise in den Dateien
