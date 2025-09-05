# Delphi Standards

[![Lang-DE](https://img.shields.io/badge/lang-DE-blue.svg)](README.de.md) [![Lang-EN](https://img.shields.io/badge/lang-EN-lightgrey.svg)](README.md)

> Beiträge in Deutsch oder Englisch sind willkommen. Die Sprachsynchronisierung erfolgt durch den Maintainer spätestens nach dem Merge.

## Übersicht

Dieses Projekt stellt eine kompakte, praxiserprobte Grundlage für einheitlichen Delphi‑Code bereit. Im Fokus stehen:

- Ein klarer, moderner Delphi Style Guide (Deutsch und Englisch)
- Eine kuratierte .gitignore‑Konfiguration speziell für Delphi

Beides hilft Teams, konsistenten Code zu schreiben und typische Reibungen zu vermeiden.

## Warum das wichtig ist

Gerade in Team‑Repos werden in der Eile häufig Dateien eingecheckt, die nicht in die Versionsverwaltung gehören (IDE‑Artefakte, Build‑Outputs, lokale Einstellungen). Das führt zu:

- unnötigen Diffs und Merge‑Konflikten
- instabilen Builds durch lokale Artefakte
- unerwünschten Seiteneffekten im Team

Die enthaltene, auf Delphi zugeschnittene .gitignore‑Datei verhindert genau das. Der Style Guide sorgt parallel für ein gemeinsames Verständnis von Struktur, Benennung und modernen Sprachfeatures.

## Inhalte

- Delphi Style Guide (DE): [Delphi Style Guide.md](Delphi%20Style%20Guide.md)
- Delphi Style Guide (EN): [Delphi Style Guide EN.md](Delphi%20Style%20Guide%20EN.md)
- Kuratierte GitIgnore‑Vorlage: [Delphi GitIgnore.txt](Delphi%20GitIgnore.txt)

Hinweis: Der Style Guide wird synchron in Deutsch und Englisch gepflegt. Änderungen sollten immer in beiden Dokumenten erfolgen.

## Schnellstart

1) Style Guide lesen und im Team vereinbaren
- Einrückung: 2 Leerzeichen; Zeilenlänge: 120 Zeichen (Formatter/Editor‑Guideline synchron halten)
- Namenskonventionen (A-/L-/F‑Präfixe, Komponenten, Konstanten, Enums mit SCOPEDENUMS)
- Moderne Features: Generics, anonyme Methoden, Inline‑Variablen (10.3+), Multiline‑Strings (12+)

2) .gitignore aktivieren
- Datei „Delphi GitIgnore.txt“ ins Repo‑Root kopieren und in „.gitignore“ umbenennen
- Projektspezifische Ergänzungen nach Bedarf vornehmen

## Mitwirken

- Issues und Pull Requests sind willkommen
- Bitte Änderungen im Style Guide stets in DE und EN spiegeln (die Synchronisierung erfolgt spätestens nach dem Merge durch den Maintainer)

## Lizenz

MIT License – siehe Hinweise in den Dateien

