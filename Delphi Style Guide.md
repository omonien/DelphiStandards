# **Delphi Style Guide**

Ein einheitlicher Coding Style ist essenziell für die Lesbarkeit, Wartbarkeit und Teamarbeit in Softwareprojekten. Er hilft dabei, Missverständnisse zu vermeiden, Fehler schneller zu finden und die Einarbeitung neuer Teammitglieder zu erleichtern. Der hier dokumentierte Style Guide basiert auf bewährten Konventionen und soll als allgemeingültiger Vorschlag für moderne Delphi-Projekte dienen.

------

## **1. Formatierung**

### **1.1 Einrückung**

Verwende 2 Leerzeichen oder einen Tab pro logischem Block.

```
procedure Example;
begin
  DoSomething;
end;
```

### **1.2 Zeilenlänge**

Maximal 120 Zeichen pro Zeile.

Der ursprüngliche Standardwert in Delphi lag bei 80 Zeichen – ein historisches Relikt aus der Zeit textbasierter Terminals. In modernen Projekten hat sich jedoch ein Wert von 120 Zeichen als neuer Quasi-Standard etabliert. Die meisten Teams und Styleguides folgen inzwischen dieser Konvention, da sie einen besseren Kompromiss zwischen Lesbarkeit und Übersichtlichkeit bietet.

### **1.3 Kommentare**

- Nutze `//` für reguläre Kommentare
- `{}` für mehrzeilige Kommentare
- `(* *)` für temporär auskommentierten Code

```
// Dies ist ein normaler Kommentar
{ Mehrzeiliger
  Kommentar }
(* Temporär auskommentierter Code *)
```

### **1.4 Compiler-Direktiven**

In geschweiften Klammern, GROSSGESCHRIEBEN und nicht eingerückt.

```
{$IFDEF DEBUG}
// Debug-Code
{$ENDIF}
```

### **1.5 Anweisungssyntax**

- Eine Anweisung pro Zeile
- `begin` und `end` immer in neuer Zeile
- Auch bei Einzeilern grundsätzlich `begin..end` verwenden, außer bei `raise`, `exit`, etc.

```
if Condition then
begin
  DoSomething;
end;

if HasError then
  raise Exception.Create('Fehler');
```

------

## **2. Benennungskonventionen**

Im gesamten Quellcode ist PascalCase (auch UpperCamelCase genannt) zu verwenden. Das bedeutet, dass zusammengesetzte Namen ohne Trennzeichen geschrieben werden und jedes Wort mit einem Großbuchstaben beginnt. Dies gilt für Methoden, Typen, Variablen, Konstanten und Parameter.

### **2.1 Methoden**

- Großschreibung am Anfang
- Verb am Anfang zur Verdeutlichung der Aktion

```
procedure SaveDocument;
procedure DrawRectangle;
```

### **2.2 Parameter**

- Präfix `A` und PascalCase

Auch bei sehr kurzen Parameternamen (z. B. `X`, `Y`, `Z`) sollte der `A`-Präfix verwendet werden, um diese im Verlauf des Codes eindeutig als Parameter identifizieren zu können. Dies erhöht die Lesbarkeit insbesondere bei komplexeren Methoden mit vielen lokalen Variablen.

```
procedure DrawRectangle(AX, AY: Integer);
```

### **2.3 Variablen**

- Lokale Variablen mit `L`-Präfix
- Member- oder Feld-Variablen mit `F`-Präfix
- Globale Variablen mit `G`-Präfix (sofern überhaupt verwendet)
- CamelCase, keine Typ-Präfixe außer bei Komponenten

```
var
  LUserName: string;
  FIsReady: Boolean;
  GAppTitle: string;
```

#### **2.3.1 Komponentennamen**

- Der Typ der Komponente wird als Präfix verwendet (PascalCase)
- Kein `F`, `L`, `G` oder ähnlicher Präfix bei Komponenteninstanzen
- Für bestimmte Komponenten haben sich historisch Konventionen etabliert:
  - `Q` für Datenbank-Queries wie `TFDQuery`, z. B. `QCustomers`
  - `DM` für Datenmodule, z. B. `DMMain`
  - `Form` für Haupt- und Unterformulare, z. B. `FormMain`

```
ButtonLogin: TButton;
EditUserName: TEdit;
FormMain: TFormMain;
QCustomers: TFDQuery;
DMMain: TDataModule;
```

### **2.4 Konstanten**

Verwende das Präfix `c` für allgemeine Konstanten, `sc` für String-Konstanten. Verwende `ALL_CAPS` nur bei Konstanten mit systemweitem oder buildrelevantem Bezug.

```
// Technische Konstante
const
  cDefaultTimeout = 5000;
  cMaxRetryCount = 3;

// UI/String-Konstante
const
  scLoginErrorMessage = 'Benutzername oder Passwort ist ungültig.';
  scFormCaption = 'Meine Anwendung';

// Systemweite Konstante (Buildbezug)
const
  APP_VERSION = '1.2.3';
```

### **2.5 Typen und Interfaces**

- Typen: `T`-Präfix
- Interfaces: `I`-Präfix

```
type
  TCustomer = class;
  IMyService = interface;
```

------

## **3. Struktur von Units**

- Interface-Abschnitt
- Implementierung
- Initialisierung / Finalisierung nur bei Bedarf

```
unit MyUnit;

interface

procedure DoWork;

implementation

procedure DoWork;
begin
  // ...
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

```
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

- `FreeAndNil` statt `.Free`
- `try..except` nur bei sinnvoller Fehlerreaktion

```
MyObject := TObject.Create;
try
  // ...
finally
  FreeAndNil(MyObject);
end;
```

### **4.2 Verzweigungen und Schleifen**

Auch Einzeiler in Bedingungen oder Schleifen sollten grundsätzlich mit `begin..end` gekapselt werden. Dies erhöht die Lesbarkeit und verhindert Fehler beim späteren Hinzufügen weiterer Anweisungen.

```
if UserLoggedIn then
begin
  ShowDashboard;
end
else
begin
  ShowLoginScreen;
end;

case DayOfWeek of
  Monday: DoMondayRoutine;
  Tuesday: DoTuesdayRoutine;
end;

for I := 1 to 10 do
begin
  if SomeCondition then
    Break; //Hier kein begin .. end
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

```
type
  TMyClass = class
  private
    FName: string;
  proteced
    function GetName: string;
    procedure SetName(const Value: string);
  public
    property Name: string read GetName write SetName;
  end;
```

------

## **6. Events**

- `On`-Präfix für Events
- Die zugehörigen Methoden sollten mit `Do` beginnen (z. B. `DoButtonClick`) und möglichst selbsterklärend sein.
- In vielen Fällen ist es sinnvoll, statt abstrakter Namen wie `DoSomething` oder `HandleX` konkrete Aktionen zu benennen – z. B. `RefreshCustomers` oder `SaveChanges`.
- Innerhalb von `OnSomething`-Methoden (z. B. `OnClick`) sollte keine komplexe Logik stehen. Diese Methoden dienen lediglich als Einstiegspunkt und sollten delegieren.

```
procedure TForm1.OnButtonClick(Sender: TObject);
begin
  DoButtonClick;
end;

procedure TForm1.DoButtonClick;
begin
  RefreshCustomers;
end;
procedure TForm1.OnButtonClick(Sender: TObject);
begin
  HandleButtonClick;
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
- Records sollten möglichst unveränderlich sein oder klar dokumentieren, wie sie verändert werden.
- Vermeide Methoden oder komplexe Logik innerhalb eines Records, außer du verwendest gezielt `record helpers`oder `record with methods`.
- Records sind ideal für DTOs, Geometrie-Typen oder einfache Wertobjekte.

```
type
  TPoint = record
    X, Y: Integer;
  end;
```

### **7.2 Enumerationen**

- Verwende für Enumerationstypen `T` als Präfix
- Bevorzuge die Punktnotation bei Zugriffen auf Enum-Werte (z. B. `TOrderStatus.New`) – dies ist lesbarer und vermeidet Namenskonflikte, insbesondere bei gleichnamigen Konstanten.
- Verwende keine redundanten Präfixe bei Enum-Werten (z. B. `osNew`) – der Typname selbst liefert bereits den Kontext. Statt `osNew` genügt `New`.
- Diese Schreibweise erfordert ggf. die Compiler-Direktive `{$SCOPEDENUMS ON}` oder die Verwendung von `type TOrderStatus = (New, Processing, Completed) scoped;`

```
type
  TOrderStatus = (New, Processing, Completed);

var
  LStatus: TOrderStatus;
begin
  LStatus := TOrderStatus.New;
end;

```

### **7.3 Thread-Sicherheit**

Für Thread-Synchronisation und den sicheren Zugriff auf gemeinsam genutzte Ressourcen sollte bevorzugt `TMonitor`verwendet werden. Im Gegensatz zu `TCriticalSection` ist `TMonitor` performanter, da es oft ohne separate Synchronisationsobjekte auskommt – jede beliebige Objektinstanz kann direkt als Lock verwendet werden.

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
