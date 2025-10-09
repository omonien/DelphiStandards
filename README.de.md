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
- Release Tools: [release-tools/](release-tools/) - Automatisierte Release-Erstellung

Hinweis: Der Style Guide wird synchron in Deutsch und Englisch gepflegt. Änderungen sollten immer in beiden Dokumenten erfolgen.

## Schnellstart

1) Style Guide lesen und im Team vereinbaren
- Einrückung: 2 Leerzeichen; Zeilenlänge: 120 Zeichen (Formatter/Editor‑Guideline synchron halten)
- Namenskonventionen (A-/L-/F‑Präfixe, Komponenten, Konstanten, Enums mit SCOPEDENUMS)
- Moderne Features: Generics, anonyme Methoden, Inline‑Variablen (10.3+), Multiline‑Strings (12+)

2) .gitignore aktivieren
- Datei „Delphi GitIgnore.txt“ ins Repo‑Root kopieren und in „.gitignore“ umbenennen
- Projektspezifische Ergänzungen nach Bedarf vornehmen

3) Git Attributes hinzufügen
- „Delphi GitAttributes.txt“ ins Repo‑Root kopieren und in „.gitattributes“ umbenennen.
- Die Datei normalisiert Zeilenenden für Delphi‑Quellen (CRLF) und markiert Binärartefakte (.res, .ico, .dcu, .bpl, .dll, .exe) als binary.
- Werden .dfm/.fmx im Projekt als Text gespeichert (Standard), die Text‑Regeln beibehalten. Bei Binärspeicherung die Binary‑Regeln aus dem Template aktivieren.
- Für bestehende Repos nach dem Hinzufügen ausführen: `git add --renormalize .` und die Änderungen committen.

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

