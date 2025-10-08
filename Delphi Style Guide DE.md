# **Delphi Style Guide**

> **Version:** 2.0
> **Autor:** Olaf Monien
> **Stand:** 2025-09-07

> Hinweis: Dieser Style Guide liegt in deutscher und englischer Fassung vor und wird synchron gepflegt. Änderungen sollten stets in beiden Dokumenten vorgenommen werden.


Ein einheitlicher Coding Style ist essenziell für die Lesbarkeit, Wartbarkeit und Teamarbeit in Softwareprojekten. Er hilft dabei, Missverständnisse zu vermeiden, Fehler schneller zu finden und die Einarbeitung neuer Teammitglieder zu erleichtern. Der hier dokumentierte Style Guide basiert auf bewährten Konventionen und soll als allgemeingültiger Vorschlag für moderne Delphi-Projekte dienen.

---

## **Quick Start - Die wichtigsten Regeln auf einen Blick**

Neu im Projekt? Hier sind die essentiellen Regeln:

### **Benennung**
```pascal
// Variablen
var
  LCustomer: TCustomer;        // Lokal: L-Präfix

type
  TMyClass = class
  private
    FName: string;             // Feld: F-Präfix
  end;

// Parameter
procedure DoSomething(const AValue: string);  // Parameter: A-Präfix

// Schleifenzähler - Ausnahme!
for var i := 0 to 10 do       // Kleinbuchstabe, kein Präfix

// Konstanten
const
  cMaxRetries = 3;             // Technisch: c-Präfix
  scErrorMessage = 'Error';    // String: sc-Präfix
```

### **Typen**
```pascal
type
  TCustomer = class end;           // Klasse: T-Präfix
  ILogger = interface end;         // Interface: I-Präfix
  TPoint = record end;             // Record: T-Präfix, KEINE F-Präfixe bei Feldern!
  TFileUtils = class sealed end;   // Utility-Klasse: sealed
  TStringHelper = record helper for string end;  // Helper: nur für echte Helper!
```

### **Fehlerbehandlung**
```pascal
// Ressourcen freigeben
LObject := TObject.Create;
try
  // Verwendung
finally
  FreeAndNil(LObject);  // Immer FreeAndNil statt .Free
end;

// Mehrere Objekte
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

### **Formatierung**
- **2 Leerzeichen** Einrückung
- **120 Zeichen** max. Zeilenlänge
- `begin..end` immer auf eigener Zeile
- Inline-Variablen bevorzugen (ab Delphi 10.3+)

### **Collections**
```pascal
// Feste Größe → TArray<T>
function GetNames: TArray<string>;

// Dynamische Liste → TList<T>
var LNumbers: TList<Integer>;

// Objekte mit Ownership → TObjectList<T>
var LCustomers: TObjectList<TCustomer>;
```

### **Dokumentation**
```pascal
/// <summary>
/// Berechnet die Summe zweier Zahlen
/// </summary>
function Add(const AValue1, AValue2: Integer): Integer;
```

**→ Für Details siehe die vollständige Dokumentation unten.**

---

## **Inhaltsverzeichnis**

- [1. Formatierung](#1-formatierung)
- [2. Benennungskonventionen](#2-benennungskonventionen)
- [3. Struktur von Units](#3-struktur-von-units)
- [4. Programmierstil](#4-programmierstil)
- [5. Properties und Getter/Setter](#5-properties-und-gettersetter)
- [6. Events](#6-events)
- [7. Verschiedenes](#7-verschiedenes)
- [8. Moderne Delphi-Features](#8-moderne-delphi-features)
- [9. Dokumentation](#9-dokumentation)
- [10. Zusammenfassung](#10-zusammenfassung)

------

## **1. Formatierung**

### **1.1 Einrückung**

Verwende **2 Leerzeichen** pro logischem Block. Tabs sind zu vermeiden, da sie in verschiedenen Editoren unterschiedlich dargestellt werden können.

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

### **1.2 Zeilenlänge**

Maximal 120 Zeichen pro Zeile.

Der ursprüngliche Standardwert in Delphi lag bei 80 Zeichen – ein historisches Relikt aus der Zeit textbasierter Terminals. In modernen Projekten hat sich jedoch ein Wert von 120 Zeichen als neuer Quasi-Standard etabliert. Die meisten Teams und Styleguides folgen inzwischen dieser Konvention, da sie einen besseren Kompromiss zwischen Lesbarkeit und Übersichtlichkeit bietet. Auf modernen Bildschirmen hat sich dies bei den allermeisten Delphi-Entwicklern als gelebter Standard etabliert.

Hinweis: Falls ein automatischer Formatter verwendet wird sowie die vertikale Orientierungslinie im Delphi-Editor genutzt wird, sollten beide Einstellungen (Zeilenlänge/Guideline) synchron konfiguriert sein, um uneinheitliche Umbrüche zu vermeiden.

### **1.3 Kommentare**

- Nutze `//` für einzeilige Kommentare
- `{}` für mehrzeilige Kommentare
- `(* *)` für temporär auskommentierten Code
- `///` für XML-Dokumentationskommentare

```pascal
// Dies ist ein normaler einzeiliger Kommentar

{ Mehrzeiliger
  Kommentar über
  mehrere Zeilen }

(* Temporär auskommentierter Code
   procedure OldMethod;
   begin
     // ...
   end; *)

/// <summary>
/// Dokumentationskommentar für eine Methode
/// </summary>
/// <param name="AValue">Beschreibung des Parameters</param>
procedure DocumentedMethod(const AValue: string);
```

### **1.4 Compiler-Direktiven**

In geschweiften Klammern, GROSSGESCHRIEBEN und nicht eingerückt. Verschachtelte Direktiven sollten zur besseren Lesbarkeit eingerückt werden.

```pascal
{$IFDEF DEBUG}
  {$IFDEF LOGGING}
  // Debug-Logging-Code
  {$ENDIF}
// Allgemeiner Debug-Code
{$ENDIF}

{$REGION 'Private Methods'}
// Implementierung
{$ENDREGION}
```

### **1.5 Anweisungssyntax**

- Eine Anweisung pro Zeile
- `begin` und `end` immer in neuer Zeile
- Auch bei Einzeilern grundsätzlich `begin..end` verwenden, außer bei einfachen Anweisungen wie `raise`, `exit`, `continue`, `break`

```pascal
// Bevorzugt - mit begin..end
if Condition then
begin
  DoSomething;
end;

// Akzeptabel bei einfachen Anweisungen
if HasError then
  raise Exception.Create('Fehler');

if not Found then
  Exit;

// Immer mit begin..end bei mehreren Anweisungen
if UserLoggedIn then
begin
  UpdateLastLogin;
  ShowDashboard;
end;
```

------

## **2. Benennungskonventionen**

Im gesamten Quellcode ist PascalCase (auch UpperCamelCase genannt) zu verwenden. Das bedeutet, dass zusammengesetzte Namen ohne Trennzeichen geschrieben werden und jedes Wort mit einem Großbuchstaben beginnt. Dies gilt für Methoden, Typen, Variablen, Konstanten und Parameter.

### **2.1 Methoden**

- PascalCase verwenden
- Verb am Anfang zur Verdeutlichung der Aktion
- Aussagekräftige Namen wählen
- Funktionen sollten das Ergebnis im Namen widerspiegeln

```pascal
// Procedures (Aktionen)
procedure SaveDocument;
procedure DrawRectangle;
procedure ValidateUserInput;

// Functions (Rückgabewerte)
function GetUserName: string;
function IsValidEmail(const AEmail: string): Boolean;
function CalculateTotalPrice: Currency;

// Vermeiden - zu unspezifisch
procedure DoSomething;  // Schlecht
procedure ProcessData;  // Besser: ValidateAndSaveData
```

### **2.2 Parameter**

- Präfix `A` und PascalCase
- `const` für unveränderliche Parameter verwenden
- `var` oder `out` explizit kennzeichnen

Auch bei sehr kurzen Parameternamen (z. B. `X`, `Y`, `Z`) sollte der `A`-Präfix verwendet werden, um diese im Verlauf des Codes eindeutig als Parameter identifizieren zu können. Dies erhöht die Lesbarkeit insbesondere bei komplexeren Methoden mit vielen lokalen Variablen.

```pascal
// Einfache Parameter
procedure DrawRectangle(AX, AY: Integer);

// Mit const für unveränderliche Parameter
procedure SaveToFile(const AFileName: string; const AData: TStringList);

// Mit var für Rückgabeparameter
procedure GetUserInfo(const AUserID: Integer; var AName: string; var AEmail: string);

// Mit out für initialisierte Rückgabeparameter
procedure TryParseInteger(const AValue: string; out AResult: Integer; out ASuccess: Boolean);
```

### **2.3 Variablen**

- Lokale Variablen mit `L`-Präfix
- Member- oder Feld-Variablen mit `F`-Präfix
- Globale Variablen mit `G`-Präfix (sollten vermieden werden)
- PascalCase, keine Typ-Präfixe außer bei Komponenten
- Aussagekräftige Namen verwenden
- **Ausnahme**: Einfache Schleifenzähler dürfen kleingeschriebene Einzelbuchstaben (`i`, `j`, `k`) ohne Präfix verwenden

```pascal
var
  LUserName: string;
  LCustomerList: TObjectList<TCustomer>;
  LIndex: Integer;
  LFound: Boolean;

// Schleifenzähler - Ausnahme von der Regel
for var i := 0 to 10 do
begin
  // Einfacher Schleifenzähler ohne L-Präfix
end;

// Verschachtelte Schleifen
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
// Globale Variablen (vermeiden!)
var
  GAppTitle: string;
  GLogLevel: Integer;
```

**Ausnahme: Unit-interne globale Variablen im Implementation-Abschnitt**

Globale Variablen im `implementation`-Abschnitt sind akzeptabel für Unit-interne Singletons oder Zustandsverwaltung:

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
  GServiceInstance: TMyService;  // Unit-intern, nicht von außen sichtbar
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

**Vorteile:**
- Nicht von außen sichtbar (Kapselung)
- Ideal für Unit-Singletons
- Vermeidet globale Verschmutzung des Namespaces

#### **2.3.1 Komponentennamen**

- Der Typ der Komponente wird als Präfix verwendet (PascalCase)
- Kein `F`, `L`, `G` oder ähnlicher Präfix bei Komponenteninstanzen
- Aussagekräftige Namen verwenden, die den Zweck beschreiben
- Für bestimmte Komponenten haben sich historisch Konventionen etabliert:
  - `Q` für Datenbank-Queries wie `TFDQuery`, z. B. `QCustomers`
  - `DM` für Datenmodule, z. B. `DMMain`
  - `Form` für Haupt- und Unterformulare, z. B. `FormMain`

```pascal
// UI-Komponenten
ButtonLogin: TButton;
ButtonCancel: TButton;
EditUserName: TEdit;
EditPassword: TEdit;
LabelWelcome: TLabel;
PanelHeader: TPanel;
GridCustomers: TStringGrid;

// Datenbank-Komponenten
QCustomers: TFDQuery;
QOrders: TFDQuery;
ConnectionMain: TFDConnection;

// Formulare und Module
FormMain: TFormMain;
FormSettings: TFormSettings;
DMMain: TDataModule;
```

### **2.4 Konstanten**

Verwende das Präfix `c` für allgemeine Konstanten, `sc` für String-Konstanten. Verwende `ALL_CAPS` nur bei Konstanten mit systemweitem oder buildrelevantem Bezug.

```pascal
// Technische Konstanten
const
  cDefaultTimeout = 5000;
  cMaxRetryCount = 3;
  cBufferSize = 1024;
  cPI = 3.14159265359;

// UI/String-Konstanten
const
  scLoginErrorMessage = 'Benutzername oder Passwort ist ungültig.';
  scFormCaption = 'Meine Anwendung';
  scConfirmDelete = 'Möchten Sie diesen Eintrag wirklich löschen?';

// Systemweite Konstanten (Buildbezug)
const
  APP_VERSION = '1.2.3';
  BUILD_NUMBER = 12345;
  COMPANY_NAME = 'Meine Firma GmbH';

// Resource-Strings (lokalisierbar)
resourcestring
  rsErrorFileNotFound = 'Die Datei wurde nicht gefunden.';
  rsConfirmExit = 'Möchten Sie die Anwendung beenden?';
```

### **2.5 Typen und Interfaces**

- Typen: `T`-Präfix
- Interfaces: `I`-Präfix
- Exceptions: `E`-Präfix
- Aussagekräftige Namen verwenden

```pascal
type
  // Klassen (Platzhalter - minimal gültige Definitionen)
  TCustomer = class end;
  TOrderManager = class end;
  TDatabaseConnection = class end;

  // Sealed Classes (Utility-Klassen ohne Instanzen)
  TPathUtils = class sealed end;
  TStringUtils = class sealed end;

  // Interfaces (Platzhalter)
  ILogger = interface end;
  IDataRepository = interface end;
  IEmailService = interface end;

  // Records (Platzhalter)
  TPoint3D = record end;
  TCustomerData = record end;

  // Exceptions
  EInvalidUserException = class(Exception);
  EDatabaseConnectionException = class(Exception);

  // Enumerationen (bevorzugt mit {$SCOPEDENUMS ON})
  TOrderStatus = (New, Processing, Completed, Cancelled);
  TLogLevel = (Debug, Info, Warning, Error);
```

------

## **3. Struktur von Units**

Eine Unit sollte eine klare, logische Struktur haben:

- Interface-Abschnitt (öffentliche Deklarationen)
- Implementation-Abschnitt (private Implementierung)
- Initialisierung / Finalisierung nur bei Bedarf
- Uses-Klauseln sinnvoll gruppieren

```pascal
unit Customer.Manager;

interface

uses
  // System-Units
  System.SysUtils, System.Classes, System.Generics.Collections,
  // Datenbank-Units
  FireDAC.Comp.Client, FireDAC.Stan.Param,
  // Eigene Units
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
  // Nur in Implementation benötigte Units
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

------

## **4. Programmierstil**

### **4.1 Fehlerbehandlung**

- `try..finally` für Ressourcenfreigabe
- `FreeAndNil` statt `.Free`
- `try..except` nur bei sinnvoller Fehlerreaktion
- Bei mehreren Instanzen empfiehlt es sich, diese vor dem `try`-Block auf `nil` zu setzen und erst **nach** dem `try` zu instanzieren. Dadurch wird sichergestellt, dass bei einem Fehler während der Instanzierung keine nicht initialisierten Objekte freigegeben werden.

```pascal
var
  LQuery: TFDQuery;
  LList: TObjectList<TSomething>;
begin
  LQuery := nil;
  LList := nil;
  try
    LQuery := TFDQuery.Create(nil);
    LList := TObjectList<TSomething>.Create(True);
    // Verwendung der Instanzen
  finally
    FreeAndNil(LQuery);
    FreeAndNil(LList);
  end;
end;
```

**Weitere Regeln:**
- `FreeAndNil` statt `.Free` verwenden
- `try..except` nur bei sinnvoller Fehlerreaktion
- Niemals leere `except`-Blöcke verwenden

```pascal
// Einfaches Beispiel
LObject := TObject.Create;
try
  // Verwendung des Objekts
finally
  FreeAndNil(LObject);
end;

// Exception-Behandlung
try
  RiskyOperation;
except
  on E: ESpecificException do
  begin
    LogError(E.Message);
    raise; // Weiterleiten wenn nötig
  end;
  on E: Exception do
  begin
    LogError('Unerwarteter Fehler: ' + E.Message);
    // Behandlung oder Weiterleitung
  end;
end;
```

**Ausnahme: Defensive Programmierung in kritischen Systemen**

In kritischen Systemen (z.B. Exception-Handler, Logging, Cleanup-Code) kann es notwendig sein, Fehler zu unterdrücken, um Rekursion oder Systemabstürze zu vermeiden:

```pascal
// Exception-Handler darf selbst keine Exceptions werfen
procedure LogException(const E: Exception);
begin
  try
    WriteToLogFile(E.Message);
  except
    // Silently ignore - logging must not fail
    // Alternative: Fallback zu OutputDebugString
  end;
end;

// Cleanup-Code in Finalization
finalization
  try
    if Assigned(GResolver) then
      FreeAndNil(GResolver);
  except
    // Silently ignore - finalization must complete
  end;
end.
```

**Wichtig:** Solche `except`-Blöcke sollten:
- Einen Kommentar haben, der erklärt, warum Fehler unterdrückt werden
- Nur in absolut notwendigen Fällen verwendet werden
- Möglichst einen Fallback-Mechanismus haben

### **4.2 Verzweigungen und Schleifen**

Auch Einzeiler in Bedingungen oder Schleifen sollten grundsätzlich mit `begin..end` gekapselt werden. Dies erhöht die Lesbarkeit und verhindert Fehler beim späteren Hinzufügen weiterer Anweisungen.

- Schleifenzähler dürfen kleingeschriebene Einzelbuchstaben (`i`, `j`, `k`) ohne `L`-Präfix verwenden

```pascal
if UserLoggedIn then
begin
  ShowDashboard;
end
else
begin
  ShowLoginScreen;
end;

case DayOfTheWeek(Date) of // 1=Montag .. 7=Sonntag
  1: DoMondayRoutine;
  2: DoTuesdayRoutine;
  // ...
end;

// Einfacher Schleifenzähler
for var i := 1 to 10 do
begin
  if SomeCondition then
    Break; // Hier kein begin..end notwendig
end;

// Traditionelle Deklaration
var
  i: Integer;
begin
  for i := 0 to List.Count - 1 do
  begin
    ProcessItem(List[i]);
  end;
end;
```

- Kein `exit` in Schleifen verwenden

### **4.3 Numerik & Typen**

- Float-Vergleiche vermeiden, nie mit `=`: Da Gleitkommawerte wie `Double` und `Single` intern in binärer Näherungsdarstellung gespeichert werden, können viele dezimale Werte – z. B. `1.99` – prinzipbedingt nicht exakt dargestellt werden. Dies ist ideal für wissenschaftliche Berechnungen, aber problematisch bei Festkomma-Anwendungen wie Geldbeträgen. Deshalb sind Vergleiche mit `=` meist unzuverlässig. Dieses Verhalten ist keine Eigenheit von Delphi, sondern ein generelles Merkmal binärer Fließkomma-Arithmetik auf heutigen Prozessorarchitekturen. Statt `A = B` sollte daher mit einer Toleranz (Epsilon) gearbeitet werden.
- `Double` oder `Single` für Gleitkommazahlen verwenden, wenn exakte Genauigkeit nicht erforderlich ist.
- Für monetäre Berechnungen sollten `Currency` oder `TBcd` (Binary Coded Decimal) verwendet werden: Diese Typen bieten eine deutlich höhere Genauigkeit und Reproduzierbarkeit, da sie nicht binär, sondern dezimal intern arbeiten. Dadurch vermeiden sie typische Rundungsprobleme bei Geldbeträgen. `TBcd` ist in der Unit `Data.FmtBcd` definiert, `Currency` ist ein eingebauter Systemtyp aus der Unit `System`. Beide sind bevorzugt bei finanziellen Berechnungen zu verwenden.
- `Variant` nur verwenden, wenn es technisch erforderlich ist (z. B. bei COM-Objekten).

------

## **5. Properties und Getter/Setter**

Eigene Getter- und Setter-Methoden sollten nur dann eingeführt werden, wenn zusätzliche Logik notwendig ist. Greifen die Methoden lediglich direkt auf die Feldvariable zu, ist die explizite Definition überflüssig und beeinträchtigt die Lesbarkeit ohne funktionalen Vorteil.

Die Feldvariablen (`F...`) sollten `private` sein, Getter und Setter idealerweise `protected`.

Beim Einführen von Getter- und Setter-Methoden sollte außerdem abgewogen werden, ob diese `virtual` deklariert werden sollen, um ein Überschreiben in abgeleiteten Klassen zu ermöglichen.

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
    // Weitere Validierung oder Benachrichtigung
  end;
end;

function TMyClass.GetDisplayName: string;
begin
  Result := Format('%s (%d Jahre)', [FName, FAge]);
end;
```

------

## **6. Events**

- `On`-Präfix für Events
- Die zugehörigen Methoden sollten mit `Do` beginnen (z. B. `DoButtonClick`) und möglichst selbsterklärend sein.
- In vielen Fällen ist es sinnvoll, statt abstrakter Namen wie `DoSomething` oder `HandleX` konkrete Aktionen zu benennen – z. B. `RefreshCustomers` oder `SaveChanges`.
- Innerhalb von `OnSomething`-Methoden (z. B. `OnClick`) sollte keine komplexe Logik stehen. Diese Methoden dienen lediglich als Einstiegspunkt und sollten delegieren.

```pascal
// Event-Handler - nur Delegation
procedure TForm1.ButtonLoginClick(Sender: TObject);
begin
  DoLogin;
end;

procedure TForm1.ButtonCancelClick(Sender: TObject);
begin
  DoCancel;
end;

// Geschäftslogik in separaten Methoden
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

------

## **7. Verschiedenes**

### **7.1 WITH-Konstrukte**

Die Verwendung des `with`-Statements ist strikt zu vermeiden.

Obwohl `with` in Delphi ursprünglich zur Reduktion redundanter Objektzugriffe gedacht war, führt es häufig zu:

- schlechter Lesbarkeit,
- unerwarteten Seiteneffekten durch verdeckte Namensauflösung,
- erhöhter Fehleranfälligkeit beim Refactoring,
- erheblichen Schwierigkeiten beim Debuggen, da nicht mehr eindeutig nachvollziehbar ist, auf welches Objekt sich ein Ausdruck bezieht.

Die explizite Angabe von Objektbezeichnern verbessert sowohl die Verständlichkeit als auch die Wartbarkeit des Codes deutlich.

**Beispiel – vermeiden:**

```
with Customer do
begin
  Name := 'Max';
  Address := 'Beispielstraße';
end;
```

**Beispiel – besser:**

```
Customer.Name := 'Max';
Customer.Address := 'Beispielstraße';
```

### **7.2 Record-Typen**

- Verwende Records für einfache, in sich geschlossene Datenstrukturen ohne komplexes Verhalten.
- Verwende `T` als Präfix für Record-Namen, analog zu Klassen.
- **Record-Felder haben KEINE Präfixe** (kein `F`, `L`, `G`) – sie sind immer öffentlich
- Vermeide Methoden oder komplexe Logik innerhalb eines Records, außer du verwendest gezielt `record helpers` oder `record with methods`.
- Records sind ideal für DTOs, Geometrie-Typen oder einfache Wertobjekte.

```pascal
type
  TPoint = record
    X, Y: Integer;  // Keine F-Präfixe bei Records!
  end;

  TStackFrameInfo = record
    ModuleName: string;
    ProcName: string;
    FileName: string;
    Line: Integer;
    Address: Pointer;
  end;
```

**Vergleich Klasse vs. Record:**

```pascal
// Klasse - private Felder mit F-Präfix
type
  TCustomer = class
  private
    FName: string;      // F-Präfix für private Felder
    FAge: Integer;
  public
    property Name: string read FName write FName;
    property Age: Integer read FAge write FAge;
  end;

// Record - öffentliche Felder ohne Präfix
type
  TCustomerData = record
    Name: string;       // Kein Präfix - immer öffentlich
    Age: Integer;
  end;
```

### **7.2a Sealed Classes für Utility-Funktionen**

Für reine Utility-Klassen ohne Instanzen und Zustand verwende `class sealed` mit ausschließlich `class`-Methoden.

**Wann `sealed class` statt `record` verwenden:**

- **Record**: Für Datenstrukturen (DTOs, Wertobjekte)
  ```pascal
  type
    TPoint = record
      X, Y: Integer;
    end;
  ```

- **Sealed Class**: Für Utility-Funktionen ohne Daten
  ```pascal
  type
    TPathUtils = class sealed
      class function FileExists(const APath: string): Boolean;
      class procedure DeleteFile(const APath: string);
    end;
  ```

**Vorteile von `sealed` bei Utility-Klassen:**

- Verhindert sinnlose Vererbung
- Kommuniziert Design-Intent: "Keine Instanzen, nur Funktionen"
- Ermöglicht Compiler-Optimierungen
- Entspricht Best Practices aus anderen Sprachen (C# `static class`, Java `final class`)

**Vergleich:**

| Aspekt | Record | Sealed Class | Normale Klasse |
|--------|--------|--------------|----------------|
| Zweck | Datenstruktur | Utility-Funktionen | Objekte mit Zustand |
| Instanzen | Wertsemantik | Keine (nur class methods) | Referenzsemantik |
| Vererbung | Nein | Nein (sealed) | Ja (möglich) |
| Beispiel | `TPoint`, `TRect` | `TPathUtils`, `TDXStacktrace` | `TCustomer`, `TOrder` |

### **7.3 Class und Record Helper**

Class und Record Helper erweitern bestehende Typen um zusätzliche Methoden, ohne den ursprünglichen Typ zu verändern. Verwende das Suffix `Helper` **ausschließlich** für echte Class und Record Helper.

**Namenskonvention:**
- Format: `T<Typname>Helper`
- Das Wort "Helper" ist **reserviert** für Class und Record Helper
- Verwende "Helper" NICHT für Sealed Utility-Klassen

```pascal
// Korrekt - Record Helper
type
  TPointHelper = record helper for TPoint
    function Distance(const AOther: TPoint): Double;
    function ToString: string;
  end;

// Korrekt - Class Helper
type
  TStringListHelper = class helper for TStringList
    procedure SaveToFileUTF8(const AFileName: string);
    function ContainsText(const AText: string): Boolean;
  end;

// FALSCH - Kein Helper, nur eine Utility-Klasse
type
  TFileHelper = class sealed  // Sollte TFileUtils o.ä. heißen
    class function FileExists(const APath: string): Boolean;
  end;

// Korrekt - Sealed Utility-Klasse
type
  TFileUtils = class sealed
    class function FileExists(const APath: string): Boolean;
    class procedure DeleteFile(const APath: string);
  end;
```

**Wann Helper verwenden:**
- Erweiterung von RTL/VCL/FMX-Typen ohne Vererbung
- Hinzufügen von Convenience-Methoden zu Records
- Rückwärtskompatibilität, wenn der ursprüngliche Typ nicht geändert werden kann

**Wichtige Hinweise:**
- Nur ein Helper kann für einen Typ in einem gegebenen Scope aktiv sein
- Helper können keine Felder hinzufügen, nur Methoden
- Helper-Methoden haben Zugriff auf private Member des erweiterten Typs

### **7.4 Enumerationen**

- Verwende für Enumerationstypen `T` als Präfix
- Bevorzuge die Punktnotation bei Zugriffen auf Enum-Werte (z. B. `TOrderStatus.New`) – dies ist lesbarer und vermeidet Namenskonflikte, insbesondere bei gleichnamigen Konstanten.
- Verwende keine redundanten Präfixe bei Enum-Werten (z. B. `osNew`) – der Typname selbst liefert bereits den Kontext. Statt `osNew` genügt `New`.
- Diese Schreibweise erfordert ggf. die Compiler-Direktive `{$SCOPEDENUMS ON}`. Eine separate `scoped`-Deklaration am Typ ist in Delphi nicht vorgesehen.

```
type
  TOrderStatus = (New, Processing, Completed);

var
  LStatus: TOrderStatus;
begin
  LStatus := TOrderStatus.New;
end;

```

### **7.5 Thread-Sicherheit**

Für Thread-Synchronisation und den sicheren Zugriff auf gemeinsam genutzte Ressourcen sollte bevorzugt `TMonitor` verwendet werden. Im Gegensatz zu `TCriticalSection` ist `TMonitor` performanter, da es oft ohne separate Synchronisationsobjekte auskommt – jede beliebige Objektinstanz kann direkt als Lock verwendet werden.

Ein typisches Muster sieht folgendermaßen aus:

```
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

Dieses Pattern gewährleistet, dass bei parallelem Zugriff kein Datenverlust oder undefiniertes Verhalten auftritt. Wichtig ist, `TMonitor.Enter` und `TMonitor.Exit` stets in einem `try..finally`-Block zu verwenden, um Deadlocks durch vergessene Freigabe zu verhindern.

#### Vergleich `TMonitor` vs. `TCriticalSection`

| Merkmal              | TMonitor                                | TCriticalSection                  |
| -------------------- | --------------------------------------- | --------------------------------- |
| Performance          | Hoch, direkt am Objekt nutzbar          | Gut, benötigt separates Objekt    |
| Speicheraufwand      | Kein zusätzliches Objekt nötig          | Separate Instanz erforderlich     |
| Einfachheit          | Sehr einfach bei Self-orientiertem Lock | Flexibler, aber mehr Codeaufwand  |
| Wiederverwendbarkeit | Lock an beliebigem Objekt möglich       | Lock-Objekt muss übergeben werden |
| Reentrant            | Ja                                      | Ja                                |

In modernen Delphi-Projekten wird `TMonitor` für einfache Szenarien klar bevorzugt.

------

## **8. Moderne Delphi-Features**

### **8.1 Generics**

Nutze Generics für typsichere Collections und wiederverwendbare Algorithmen.

```pascal
// Typsichere Collections
var
  LCustomers: TObjectList<TCustomer>;
  LNames: TList<string>;
  LLookup: TDictionary<string, TCustomer>;

// Generische Methoden
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

#### **8.1.1 TArray<T> vs. TList<T> vs. TObjectList<T>**

Wähle den richtigen Collection-Typ basierend auf dem Anwendungsfall:

**TArray<T>** - Für Collections mit fester oder selten wechselnder Größe:
```pascal
type
  TStacktrace = TArray<TStackFrameInfo>;  // Feste Größe nach Erfassung

function GetTopCustomers: TArray<TCustomer>;  // Rückgabewert

var
  LNames: TArray<string>;
begin
  SetLength(LNames, 3);
  LNames[0] := 'Alice';   // O(1) - sehr schnell, keine Reallokation
  LNames[1] := 'Bob';
  LNames[2] := 'Charlie';
  LNames[0] := 'John';    // Ändern von Elementen ist O(1)
end;
```

**Vorteile:**
- Geringer Memory-Overhead
- Sehr schneller Zugriff und Änderung von Elementen (O(1))
- Ideal für Rückgabewerte und Collections mit fester Größe
- Keine Speicherverwaltung nötig (automatisch freigegeben)
- Elemente können effizient in-place geändert werden

**TList<T>** - Für dynamische Listen von Werttypen:
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

**Vorteile:**
- Dynamisches Hinzufügen/Entfernen
- Sortierung und Suche eingebaut
- Ideal für Werttypen (Integer, String, Records)

**TObjectList<T>** - Für Listen von Objekten mit Ownership:
```pascal
var
  LCustomers: TObjectList<TCustomer>;
begin
  LCustomers := TObjectList<TCustomer>.Create(True);  // True = OwnsObjects
  try
    LCustomers.Add(TCustomer.Create('Max'));
    // Objekte werden automatisch freigegeben
  finally
    FreeAndNil(LCustomers);
  end;
end;
```

**Vorteile:**
- Automatische Speicherverwaltung für Objekte (wenn OwnsObjects = True)
- Verhindert Memory Leaks
- Ideal für Objektsammlungen

**Entscheidungshilfe:**

| Kriterium | TArray<T> | TList<T> | TObjectList<T> |
|-----------|-----------|----------|----------------|
| Größe ändern | Selten (SetLength) | Häufig (Add/Delete) | Häufig (Add/Delete) |
| Element-Änderung | Sehr schnell (O(1)) | Schnell (O(1)) | Schnell (O(1)) |
| Inhalt | Beliebige Typen | Werttypen | Objekte |
| Ownership | N/A | N/A | Ja (OwnsObjects) |
| Memory-Overhead | Minimal | Mittel | Mittel |
| Anwendungsfall | Collections mit fester Größe, Rückgabewerte | Dynamische Listen | Objektsammlungen |

### **8.2 Anonyme Methoden**

Verwende anonyme Methoden für kurze, lokale Funktionalität.

```pascal
// Anonyme Methoden für Event-Handler
var
  LButton: TButton;
begin
  LButton := TButton.Create(Self);
  LButton.OnClick := procedure(Sender: TObject)
    begin
      ShowMessage('Button wurde geklickt!');
    end;
end;
```

### **8.3 Inline Variables (Delphi 10.3+)**

Deklariere Variablen direkt am Verwendungsort für bessere Lesbarkeit. Bei Schleifen sollte die Inline-Deklaration bevorzugt werden.

```pascal
// Traditionell
procedure ProcessData;
var
  i: Integer;
  LCustomer: TCustomer;
begin
  for i := 0 to CustomerList.Count - 1 do
  begin
    LCustomer := CustomerList[i];
    // Verarbeitung...
  end;
end;

// Mit Inline Variablen (bevorzugt ab Delphi 10.3+)
procedure ProcessData;
begin
  for var i := 0 to CustomerList.Count - 1 do
  begin
    var LCustomer := CustomerList[i];
    // Verarbeitung...
  end;

  // Auch bei anderen Variablen sinnvoll
  var LResult := CalculateSomething;
  if LResult > 0 then
    ProcessResult(LResult);
end;
```

### **8.4 Multiline Strings (Delphi 12+)**

Verwende Multiline-String-Literale für bessere Lesbarkeit bei längeren Texten. Die Einrückung der schließenden `'''` bestimmt die Basis-Einrückung des gesamten Strings.

```pascal
// Traditionell - unübersichtlich
const
  SQL_QUERY = 'SELECT c.id, c.name, c.email, o.order_date ' +
              'FROM customers c ' +
              'LEFT JOIN orders o ON c.id = o.customer_id ' +
              'WHERE c.active = 1 ' +
              'ORDER BY c.name';

// Mit Multiline Strings (Delphi 12+)
const
  SQL_QUERY = '''
    SELECT c.id, c.name, c.email, o.order_date
    FROM customers c
    LEFT JOIN orders o ON c.id = o.customer_id
    WHERE c.active = 1
    ORDER BY c.name
    ''';

// JSON Template mit Einrückung
const
  JSON_TEMPLATE = '''
    {
      "customer": {
        "id": %d,
        "name": "%s",
        "email": "%s"
      }
    }
    ''';

// HTML Template
const
  HTML_TEMPLATE = '''
  <div class="customer-card">
    <h2>%s</h2>
    <p>Email: %s</p>
    <p>ID: %d</p>
  </div>
  ''';

// Inline-Verwendung
var
  LMessage: string;
begin
  LMessage := '''
  Sehr geehrte Damen und Herren,

  hiermit bestätigen wir Ihre Bestellung.

  Mit freundlichen Grüßen
  Ihr Team
  ''';
end;
```

**Wichtige Regeln für Multiline Strings:**
- Öffnende `'''` muss von einer neuen Zeile gefolgt werden
- Schließende `'''` muss in einer eigenen Zeile stehen
- Die Einrückung der schließenden `'''` bestimmt die Basis-Einrückung
- Alle Zeilen müssen mindestens so weit eingerückt sein wie die schließende `'''`
- Die letzte Newline vor der schließenden `'''` wird weggelassen

### **8.5 Attributes**

Nutze Attributes für Metadaten und Konfiguration. Attribute sollten in der Regel über den jeweiligen Klassen, Feldern oder Methoden definiert werden, um die Lesbarkeit und Wartbarkeit zu verbessern.

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


## **9. Dokumentation**

Verwende XML-Dokumentationskommentare (`///`) konsequent für **alle** öffentlichen APIs:
- Alle öffentlichen Klassen, Records und Interfaces
- Alle öffentlichen Methoden und Funktionen
- Alle öffentlichen Properties
- Alle öffentlichen Typen und Konstanten

**Dokumentationsebenen:**
- **Minimum:** `<summary>` für alle öffentlichen Elemente
- **Optimal:** Zusätzlich `<param>` für alle Parameter, `<returns>` für Funktionen mit Rückgabewert, `<exception>` für dokumentierte Exceptions und `<remarks>` für zusätzliche Hinweise, wenn hilfreich

```pascal
/// <summary>
/// Berechnet die Entfernung zwischen zwei Punkten
/// </summary>
/// <param name="APoint1">Erster Punkt</param>
/// <param name="AParam2">Zweiter Punkt</param>
/// <returns>Entfernung als Double-Wert</returns>
/// <exception cref="EArgumentException">
/// Wird ausgelöst, wenn einer der Punkte nil ist
/// </exception>
function CalculateDistance(const APoint1, APoint2: TPoint): Double;

/// <summary>
/// Repräsentiert einen Kunden im System
/// </summary>
/// <remarks>
/// Diese Klasse kapselt alle kundenbezogenen Daten und Operationen.
/// Verwende die Factory-Methode CreateCustomer für die Instanzierung.
/// </remarks>
type
  TCustomer = class
  private
    FID: Integer;
    FName: string;
  public
    /// <summary>Eindeutige Kunden-ID</summary>
    property ID: Integer read FID write FID;

    /// <summary>Vollständiger Name des Kunden</summary>
    property Name: string read FName write FName;
  end;

/// <summary>
/// Informationen über einen einzelnen Stack-Frame
/// </summary>
type
  TStackFrameInfo = record
    /// <summary>Name des Moduls (EXE/DLL)</summary>
    ModuleName: string;
    /// <summary>Name der Prozedur/Funktion</summary>
    ProcName: string;
    /// <summary>Pfad zur Quelldatei</summary>
    FileName: string;
    /// <summary>Zeilennummer in der Quelldatei</summary>
    Line: Integer;
  end;
```

------

## **10. Zusammenfassung**

Dieser Style Guide definiert einheitliche Standards für:

- **Formatierung**: Konsistente Einrückung, Zeilenlängen und Strukturierung
- **Namenskonventionen**: Klare Präfixe und aussagekräftige Namen
- **Code-Organisation**: Saubere Unit-Struktur und Trennung von Verantwortlichkeiten
- **Moderne Features**: Nutzung von Generics, anonyme Methoden, Inline-Variablen (10.3+), Multiline-Strings (12+) und Attributes

Die Einhaltung dieser Konventionen führt zu:
- Besserer Lesbarkeit und Wartbarkeit
- Reduzierter Einarbeitungszeit für neue Teammitglieder
- Weniger Fehlern durch konsistente Patterns
- Professionellerem und einheitlichem Code

------

## **Lizenz**

Dieser Style Guide steht unter der MIT Lizenz.
[MIT License](https://opensource.org/licenses/MIT)
