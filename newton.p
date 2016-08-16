PROGRAM AnalysatorII;

{$incl"req.lib","intuition.lib","graphics.lib","libraries/dos.h"}

{$ opt b- }

CONST
  s_Hoehe    = 256;
  s_Breite   = 640;
  s_Tiefe    = 3;
  w_Hoehe    = 60;
  w_Breite   = s_Breite;
  GadAnz     = 22;
  MenuAnz    = 6;
  ItemAnz    = 39;

  forX       = TRUE;
  forY       = FALSE;

TYPE
  atyp   = Double;
  MyStr  = STRING[80];
  Buffer = RECORD
             s: MyStr;
             p: INTEGER
           END;

  FarbTyp = RECORD;
              Funktion,
              ErsteAbl,
              ZweiteAbl,
              DritteAbl,
              Koord,
              Tangente,
              WerteF : LONG
            END; (* RECORD *)

  Directoryname = String[DSize];
  Filename      = String[Fchars];
  Pathname      = String[162];

  AusgModusTyp = RECORD;
                   Drucker,
                   Bildschirm,
                   Grafik : BOOLEAN
                 END; (* RECORD *)

  Koords = RECORD;
    X,Y : atyp;
  END; (* RECORD *)

  WKoords = RECORD;
    X,Y : WORD;
  END; (* RECORD *)

  F_FileStruct = RECORD;
    F_Kennung     : STRING[4];
    F_MinX        : ATyp;
    F_MaxX        : ATyp;
    F_MinY        : ATyp;
    F_MaxY        : ATyp;
    F_FadenX      : WORD;
    F_FadenY      : WORD;
    F_Funktion    : STRING[80];
    F_Startwert   : ATyp;
    F_Genauigkeit : ATyp
  END; (* RECORD *)

VAR
  f           : ARRAY['f'..'h',0..99] OF Ptr; IMPORT;
  x, y        : atyp;
  v           : ARRAY['a'..'d',0..99] OF atyp; IMPORT;
  c           : CHAR;            IMPORT;
  i           : INTEGER;
  fx          : Ptr;
  b, prev     : BOOLEAN;
  everr       : BOOLEAN;         IMPORT;
  Ein         : Buffer;          EXPORT;

  FehlerStr   : STRING[80];

  Funktion    : STRING[80];
  TempStr     : STRING[80];
  RStr        : STRING[50];

  Min, Max    : Koords;

  Delta       : Koords;

  Faden       : WKoords;

  Wert        : LONG;
  AWert       : atyp;
  Startwert   : atyp;
  Iteration   : Integer;
  Genauigkeit : atyp;
  ZoomWert    : atyp;

  InString    : STRING[80];
  UserText    : STRING[80];

  Win         : NewWindow;

  scrptr      : ^Screen;
  winptr      : ^Window;
  scrwin      : ^Window;
  Msg,Msg2    : ^Intuimessage;
  ScrName,
  Verzeichnis : Directoryname;
  DateiNam    : Filename;
  Pfad        : Pathname;
  Erfolg,Knopf: Long;
  FReq        : FileRequester;

  BoolGad     : ARRAY [1..GadAnz] OF GadgetBlock;
  StrGad      : StringBlock;
  Undo        : String[81];
  FunkText    : String[81];
  Datei       : FILE OF Byte;

  ok          : BOOLEAN;
  Ende,SetEnde: BOOLEAN;
  DispMode    : BOOLEAN;

  TanSchar    : BOOLEAN;

  HauptMenu   : ARRAY [1..MenuAnz] OF Menu;
  MenuPunkt   : ARRAY [1..ItemAnz] OF MenuItem;
  MenuPunktTxt: ARRAY [1..ItemAnz] OF IntuiText;
  menue,
  menitem,
  subitem     : integer;

  Col         : Long;
  Farbe       : FarbTyp;

  Ausg        : AusgModusTyp;

  Miadr       : ^Gadget;

  process_ptr : p_MyProcess;
  oldwindow_ptr:Ptr;


PROCEDURE Inp(Var t:Buffer;  Var  f:Ptr);            IMPORT;
FUNCTION  konstant(f:Ptr):boolean;                   IMPORT;
FUNCTION  eval(f:Ptr;  r:atyp):atyp;                 IMPORT;
PROCEDURE Forget(Var f:Ptr);                         IMPORT;
PROCEDURE InFix(f:Ptr;  i:integer; VAR InString:Str);IMPORT;
FUNCTION  sgn(r:atyp):integer;                       IMPORT;
PROCEDURE InitAnalysis;                              IMPORT;
PROCEDURE ExitAnalysis;                              IMPORT;


PROCEDURE ZeigStatus(Fehler:Str);
BEGIN
  FehlerStr:="Status: "+Fehler+CHR(0);
  SetWindowTitles(winptr,FehlerStr,FehlerStr)
END; (* ZeigStatus *)


PROCEDURE TextOut(OutX, OutY : Integer; Txt : Str);
VAR Itext : Intuitext;
BEGIN
  Itext:=IntuiText(1,0,1,0,0,NIL,Txt,NIL);
  PrintIText(winptr^.RPort,^Itext,OutX,OutY)
END; (* TextOut *)


PROCEDURE ErmittleDeltaXY(VAR Delt:Koords;breit,hoch:INTEGER);
BEGIN
  IF Max.X-Min.X<>0 THEN Delt.X:=breit/(Max.X-Min.X)
                    ELSE Delt.X:=breit/(Max.X-Min.X+Genauigkeit);
  IF Max.Y-Min.Y<>0 THEN Delt.Y:=hoch/(Max.Y-Min.Y)
                    ELSE Delt.Y:=hoch/(Max.Y-Min.Y+Genauigkeit)
END; (* ErmittleDeltaXY *)


FUNCTION Umrechnen(AbsWert,Delt:atyp;was:BOOLEAN):atyp;
BEGIN
  Umrechnen:=0;
  IF Delt<>0 THEN IF was=forX THEN Umrechnen:=Min.X+(AbsWert/Delt)
                              ELSE Umrechnen:=Max.Y-(AbsWert/Delt)
END; (* Umrechnen *)


PROCEDURE ZeigXY;
BEGIN
  RStr:=RealStr(Umrechnen(Faden.X,Delta.X,forX),2);
  TextOut(560,10,RStr);
  RStr:=RealStr(Umrechnen(Faden.Y,Delta.Y,forY),2);
  TextOut(600,10,RStr)
END; (* ZeigXY *)


PROCEDURE ZeigStartwert(x0:atyp);
BEGIN
  RStr:=RealStr(x0,4);
  TextOut(570,22,RStr)
END; (* ZeigStartwert *)


PROCEDURE ErmittleStartwert;
VAR x0 : atyp;
BEGIN
  REPEAT
    IF Msg2<>NIL THEN Reply_Msg(Msg2);
    IF Msg2=NIL THEN Msg2:=p_intuimessage(Get_Msg(scrwin^.userport));
    x0:=Umrechnen(scrwin^.MouseX,Delta.X,forX);
    ZeigStartwert(x0)
  UNTIL Msg^.class = MouseButtons;
  Startwert:=x0
END; (* ErmittleStartwert *)

PROCEDURE Tausche(VAR a, b : atyp);
VAR h : atyp;
BEGIN
  h:=a;
  a:=b;
  b:=h
END; (* Tausche *)


PROCEDURE ControlMinMaxXY;
BEGIN
  IF Min.X=Max.X THEN Max.X:=Max.X+Genauigkeit;
  IF Min.Y=Max.Y THEN Max.Y:=Max.Y+Genauigkeit;
  IF Min.X>Max.X THEN Tausche(Max.X,Min.X);
  IF Min.Y>Max.Y THEN Tausche(Max.Y,Min.Y)
END (* ControlMinMaxXY *);


PROCEDURE KoordSyst(breit,hoch:Integer);
{ lösche Bildschirm und zeichne Koordinatensystem }
VAR x, y, x0, y0, xs, ys : INTEGER;
    xt, yt               : atyp;
    i                    : INTEGER;
    s                    : STRING;
    er                   : BOOLEAN;

  FUNCTION Log10(x:atyp): atyp;
  BEGIN
    Log10 := ln(x)/ln(10)
  END; (* Log10 *)

  FUNCTION Skalierung(range: atyp; dots:integer):atyp;
  VAR r,s,t: atyp;
  BEGIN
    r := range * 30 / dots;  { Anzahl Einheiten auf 30 Bildpunkten }
    s := Log10(r);
    IF s >= 0  THEN t:=  Frac(s)
               ELSE t:=1-Frac(s);
    IF t < 0.3 THEN Skalierung := 5*DbPwr10(Trunc(s))
               ELSE Skalierung :=   DbPwr10(Trunc(s))
  END; (* Skalierung *)

BEGIN
  SetRast(scrwin^.RPort,0); (* Bildschirm mit Hintergrundsfarbe löschen *)
  ZeigStatus("Ich zeichne gerade das Koordinatensystem");
  ErmittleDeltaXY(Delta,breit,hoch);
  SetDrMd(scrwin^.RPort,JAM1);
  SetAPen(scrwin^.RPort,Farbe.Koord);
  IF Max.Y-Min.Y<>0 THEN y0 := Round (hoch*Max.Y/(Max.Y-Min.Y))
                    ELSE y0 := Round (hoch*Max.Y/(Max.Y-Min.Y+Genauigkeit));
  IF Max.Y < 0 THEN ys := 2
  ELSE
  IF Min.Y > 0 THEN ys := hoch-2
  ELSE BEGIN {BEGIN}
    ys := y0;
    Move(scrwin^.RPort,0,y0);
    Draw(scrwin^.RPort,breit-1,y0)
  END;
  IF Max.X-Min.X<>0 THEN x0 := Round (-breit*Min.X/(Max.X-Min.X))
                    ELSE x0 := Round (-breit*Min.X/(Max.X-Min.X+Genauigkeit));
  IF Max.X < 0 THEN xs := breit-3
  ELSE
  IF Min.X > 0 THEN xs := 3
  ELSE BEGIN
    xs := x0;
    Move(scrwin^.RPort,x0,0);
    Draw(scrwin^.RPort,x0,hoch-1)
  END; (* ELSE *)
  xt := Skalierung(Max.X-Min.X, breit);
  FOR i:=Round(Min.X/xt) TO Round(Max.X/xt) DO BEGIN
    x := x0+Round(Delta.X*i*xt);
    Move (scrwin^.RPort, x, ys-2 );
    Draw (scrwin^.RPort, x, ys+2 );
    s := RealStr(i*xt,0);
    IF s[1]=' ' THEN Delete(s,1,1);
    IF ys+12 < hoch THEN Move (scrwin^.RPort, x-4*Length(s), ys+12)
                    ELSE Move (scrwin^.RPort, x-4*Length(s), ys-4);
    IF i<>0 THEN er:=GfxText(scrwin^.RPort, s, Length(s))
  END; (* FOR *)
  yt := Skalierung(Max.Y-Min.Y, hoch*2);
  FOR i:=Round(Min.Y/yt) TO ROUND(Max.Y/yt) DO BEGIN
    y := y0-round(Delta.Y*i*yt);
    Move (scrwin^.RPort, xs-3, y);
    Draw (scrwin^.RPort, xs+3, y);
    s := RealStr(i*yt,0);
    IF s[1]=' ' THEN Delete(s,1,1);
    IF xs > 4+8*Length(s) THEN Move (scrwin^.RPort, xs-4-8*Length(s), y+4)
                          ELSE Move (scrwin^.RPort, xs+6, y+4);
    IF i<>0 THEN er := GfxText (scrwin^.RPort, s, Length(s))
  END (* FOR *)
END; (* KoordSyst *)


PROCEDURE Graph(fx: Ptr; breit,hoch:Integer; Farb : LONG);
VAR x, y, step     : atyp;
    x0, y0, xs, ys : INTEGER;
BEGIN
  ZeigStatus("Ich zeichne gerade die Funktion/Tangente! (Das kann dauern!)");
  ErmittleDeltaXY(Delta,breit,hoch);
  SetDrMd(scrwin^.RPort, JAM1);
  SetAPen(scrwin^.RPort, Farb);
  IF Max.Y-Min.Y<>0 THEN y0 := Round (hoch*Max.Y/(Max.Y-Min.Y))
                    ELSE y0 := Round (hoch*Max.Y/(Max.Y-Min.Y+Genauigkeit));
  IF Max.X-Min.X<>0 THEN x0 := Round (-breit*Min.X/(Max.X-Min.X))
                    ELSE x0 := Round (-breit*Min.X/(Max.X-Min.X+Genauigkeit));
  x := Min.X;
  step := 0.5*(Max.X-Min.X)/breit;
  prev := false;
  REPEAT
    everr := false;
    y := eval(fx,x);
    IF everr OR (y>Max.Y) OR (y<Min.Y) THEN
      prev := false
    ELSE BEGIN
      xs := x0 + round( Delta.X*x );
      ys := y0 - round( Delta.Y*y );
      IF not prev THEN move(scrwin^.RPort,xs,ys);
      Draw (scrwin^.RPort,xs,ys);
      prev := true;
    END; (* ELSE *)
    x := x+step
  UNTIL x>=Max.X
END; (* Graph *)


FUNCTION TRequest(Titel,Frage,pos,mitte,neg:Str):Long;
VAR TextReq : TRStructure;
BEGIN
  WITH TextReq DO BEGIN
    Text         := Frage;
    Controls     := NIL;
    Window       := NIL;
    MiddleText   := mitte;
    PositiveText := pos;
    NegativeText := neg;
    Title        := Titel;
    Keymask      := $FFFF;
    textcolor    := 1;
    detailcolor  := 2;
    blockcolor   := 3;
    versionnumber:= 0;
    rfu1         := 0;
    rfu2         := 0
  END; (* WITH *)
  TRequest:=TextRequest(^TextReq)
END; (* TRequest *)


PROCEDURE Ausgeben(Txt:String); {Drucker / Bildschirm / Grafik}
VAR pr           : FILE OF CHAR;
    Zeichenkette : STRING[80];
    GfxY         : WORD;

  PROCEDURE schreibeText(GX,GY:WORD;Zeichenkette:STRING);
  BEGIN
    Move(scrwin^.RPort,GX,GY);
    ok:=GfxText(scrwin^.RPort,Zeichenkette,length(Zeichenkette))
  END; (* schreibeText *)

BEGIN
  WITH Ausg DO BEGIN
    IF Drucker THEN BEGIN
{      IF oeffne_Datei("PRT:",pr)=TRUE THEN BEGIN
        ZeigStatus("Ich drucke gerade den Wert aus!");
        write(pr,Txt);
        schliesse_Datei(pr)
      END ELSE ZeigStatus("Irgendetwas stimmt mit dem Drucker nicht...");
}
    END; (* IF *)
    IF Bildschirm THEN BEGIN
      ZeigStatus("Na? Hier has'te deine(n) Wert(e)!");
      Knopf:=TRequest("Wert:",Txt,"Versteh' ich nich'",NIL,"alles klar!");
      CASE Knopf OF
        0: Knopf:=TRequest("Bewertung:","Na also! Weiter so!",NIL,NIL,"OK!");
        1: Knopf:=TRequest("Bewertung:","Tja! Pech, ne?","Schade!",NIL,NIL);
      ELSE END (* CASE *)
    END; (* IF *)
    IF Grafik THEN BEGIN
      ZeigStatus("Ich gebe gerade den Text ab der Fadenkreuz-Position aus!");
      SetDrMd(scrwin^.RPort,JAM1);
      SetAPen(scrwin^.RPort,Farbe.WerteF);
      Zeichenkette:="";
      GfxY:=Faden.Y;
      FOR i:=1 TO length(Txt) DO BEGIN
        IF (Txt[i]<>CHR(10)) OR (Txt[i]<>CHR(0)) THEN
          Zeichenkette:=Zeichenkette+Txt[i]
        ELSE BEGIN
          schreibeText(Faden.X,GfxY,Zeichenkette);
          GfxY:=GfxY+10;
          Zeichenkette:=""
        END (* ELSE *)
      END (* FOR *)
    END (* IF *)
  END (* WITH *)
END; (* Ausgeben *)


PROCEDURE ZeigAbleitungen;

  PROCEDURE ZeigAbl(Zeile : Integer; AblTxt : Str);
  VAR Abl : Str;
  BEGIN
    Ein.s:=AblTxt;
    Ein.p:=1;
    Inp (Ein,fx);
      IF fx<>Nil THEN BEGIN
      InFix(fx,0,Abl);
      TextOut(160+7*8,Zeile,Abl);
      Forget (fx);
    END (* IF *)
  END; (* ZeigAbl *)

BEGIN
  ZeigAbl(22,"f'(x)"  );
  ZeigAbl(32,"f''(x)" );
  ZeigAbl(42,"f'''(x)")
END; (* ZeigAbleitungen *)


PROCEDURE RefreshAll;
BEGIN
{  IF Funktion<>FunkText THEN BEGIN
    Funktion:=FunkText;
    f['f',0]:=^Funktion;
    ZeigAbleitungen
  END (* IF *)}
END; (* RefreshAll *)


FUNCTION HolZahl (Titel:Str;min,max,default:Long;VAR Resultat:Long):Boolean;
VAR GZahl : GetLongStruct;
BEGIN
  ZeigStatus("Ich schätze, daß Du Zahlen eingeben mußt!");
  WITH GZahl DO BEGIN
    titlebar     := Titel;
    defaultval   := default;
    Minlimit     := min;
    Maxlimit     := max;
    Result       := 0;
    Window       := NIL;
    versionnumber:= 0;
    flags        := 0;
    rfu2         := 0
  END; (* WITH *)
  HolZahl:=GetLong(^GZahl);
  Resultat:=GZahl.result
END; (* HolZahl *)


PROCEDURE Reset_FileRequester;
BEGIN
  FReq:=FileRequester(
    0,                  {Versionsnummer}
    "",                 {Titel}
    NIL,                {Dir}
    NIL,                {File}
    NIL,                {PathName}
    NIL,                {Window}
    0,                  {MaxExtendedSelect}
    10,                 {numlines}
    20,                 {numcolumns}
    5,                  {devcolumns}
    0,                  {Flags}
    3,                  {dirnamecolor}
    1,                  {filenamecolor}
    1,                  {devicenamecolor}
    2,                  {fontnamecolor}
    2,                  {fontsizecolor}
    2,                  {detailcolor}
    1,                  {blockcolor}
    1,                  {gadgettextcolor}
    3,                  {textmessagecolor}
    1,                  {stringnamecolor}
    1,                  {stringgadgetcolor}
    1,                  {boxbordercolor}
    3,                  {gadgetboxcolor}
    chr(0),             {RFU_Stuff}
    datestamp(0,0,0),   {DirDateStamp}
    0,                  {WindowLeftEdge}
    0,                  {WindowTopEdge}
    0,                  {FontYSize}
    0,                  {FontStyle}
    NIL,                {Extended Select}
    "",                 {Hide}
    "",                 {Show}
    0,                  {FileBufferPos}
    0,                  {FileDispPos}
    0,                  {DirBufferPos}
    0,                  {DirDispPos}
    0,                  {HideBufferPos}
    0,                  {HideDispPos}
    0,                  {ShowBufferPos}
    0,                  {ShowDispPos}
    NIL,                {Memory}
    NIL,                {Memory2}
    NIL,                {Lock}
    "",                 {PrivateDirBuffer}
    NIL,                {FileInfoBlock}
    0,                  {NumEntries}
    0,                  {NumHiddenEntries}
    0,                  {filestartnumber}
    0                   {devicestartnumber}
  );
END; (* Reset_FileRequester *)


PROCEDURE Init_FileRequester(Titel,Dir,Dat:Str);
BEGIN
  Verzeichnis:=Dir;
  DateiNam:=Dat;
  Pfad:="";
  FReq.Title:=Titel;
  FReq.Dirname:=^Verzeichnis;
  FReq.Filename:=^DateiNam;
  FReq.Pathname:=^Pfad
END; (* Init_FileRequester *)


FUNCTION Existiert(DateiNam:Pathname):BOOLEAN;
VAR Exist : BPTR;
BEGIN
  Exist:=DosOpen(DateiNam,MODE_OLDFILE);
  IF Exist=0 THEN Existiert:=FALSE
             ELSE BEGIN
               Existiert:=TRUE;
               DosClose(Exist)
             END (* ELSE *)
END; (* Existiert *)


PROCEDURE LadeFunktion;

  PROCEDURE LeseFunktion;
  VAR RFile   : FILE OF F_FileStruct;
      L_Funk  : F_FileStruct;

    PROCEDURE SetzeNeueFunktion;
    BEGIN
      FunkText:=Funktion;
      RefreshAll
    END; (* SetzeNeueFunktion *)

  BEGIN
    Assign(RFile,Pfad);
    IF IoErr=0 THEN BEGIN
      Reset(RFile);
      Read(RFile,L_Funk);
      WITH L_Funk DO BEGIN
        IF F_Kennung='FUNC' THEN BEGIN
          Min.X      :=F_MinX;
          Max.X      :=F_MaxX;
          Min.Y      :=F_MinY;
          Max.Y      :=F_MaxY;
          Faden.X    :=F_FadenX;
          Faden.Y    :=F_FadenY;
          Funktion   :=F_Funktion;
          Startwert  :=F_Startwert;
          Genauigkeit:=F_Genauigkeit;
          SetzeNeueFunktion
        END (* IF *)
      END; (* WITH *)
      Close(RFile)
    END (* IF *)
  END; (* LeseFunktion *)

BEGIN
  ZeigStatus("Jetzt mußt DU eine zu ladende Funktions-Datei auswählen!");
  Init_FileRequester("Funktion laden ...",Verzeichnis,DateiNam);
  FReq.Flags:=FRQLoading;
  FReq.Show:="*.func";

  IF (FileRequest(^FReq)<>0) AND (Existiert(Pfad)=TRUE) THEN LeseFunktion
END; (* LadeFunktion *)


PROCEDURE SpeichereFunktion;

  PROCEDURE SchreibeFunktion;
  VAR RFile   : FILE OF F_FileStruct;
      S_Funk  : F_FileStruct;
  BEGIN
    Assign(RFile,Pfad);
    IF IoErr=0 THEN BEGIN
      Reset(RFile);
      WITH S_Funk DO BEGIN
        F_Kennung    :='FUNC';
        F_MinX       :=Min.X;
        F_MaxX       :=Max.X;
        F_MinY       :=Min.Y;
        F_MaxY       :=Max.Y;
        F_FadenX     :=Faden.X;
        F_FadenY     :=Faden.Y;
        F_Funktion   :=Funktion;
        F_Startwert  :=Startwert;
        F_Genauigkeit:=Genauigkeit
      END; (* WITH *)
      Write(RFile,S_Funk);
      Close(RFile)
    END (* IF *)
  END; (* SchreibeFunktion *)

BEGIN
  ZeigStatus("Jetzt mußt DU eine zu speichernde Funktions-Datei angeben!");
  Init_FileRequester("Funktion speichern ...",Verzeichnis,DateiNam);
  FReq.Flags:=FRQSaving;
  FReq.Show:="*.func";

  IF FileRequest(^FReq)<>0 THEN
    IF Existiert(Pfad)=TRUE THEN BEGIN
      Knopf:=TRequest("ACHTUNG !!!","Datei existiert schon!","Überschreiben",NIL,"Hilfe! Bloß nicht!");
      IF Knopf=1 THEN SchreibeFunktion;
    END ELSE SchreibeFunktion
END; (* SpeichereFunktion *)


PROCEDURE KonfigLaden;
BEGIN
END; (* KonfigLaden *)

PROCEDURE KonfigSpeichern;
BEGIN
END; (* KonfigSpeichern *)

PROCEDURE KonfigDefault;
BEGIN
END; (* KonfigDefault *)

PROCEDURE GrafikDrucken;
BEGIN
END; (* GrafikDrucken *)

PROCEDURE IFFSpeichern;
BEGIN
END; (* IFFSpeichern *)


PROCEDURE InitGadget(num:Word;VAR Gad : GadgetBlock);
BEGIN
  Gad.gadget:=gadget(NIL,0,0,0,0,0,0,0,NIL,NIL,NIL,0,NIL,num,0);
  Gad.border:=Border(0,0,0,0,0,0,NIL,NIL);
  Gad.text  :=IntuiText(0,0,0,0,0,NIL,"",NIL)
END; (* InitGadget *)


PROCEDURE InitStringGadget(num:Word;VAR SGad:StringBlock);
BEGIN                                                    {num}
  SGad.gadget:=gadget(NIL,0,0,0,0,0,0,0,NIL,NIL,NIL,0,NIL, 0 ,0);
  SGad.info  :=stringinfo(NIL,NIL,0,0,0,0,0,0,0,0,NIL,0,NIL);
  SGad.border:=Border(0,0,0,0,0,0,NIL,NIL);
  FOR i:=0 TO numpairs DO SGad.pairs[i]:=0
END; (* InitStringGadget *)


PROCEDURE ErstelleGadgets;
BEGIN
  FOR i:=1 TO GadAnz DO InitGadget(i,BoolGad[i]);
  InitStringGadget(1,StrGad);
  FunkText:="";
  Undo:="";
  LinkGadget(^BoolGad[ 1],"f(x)   "     ,^Win,160,12); {plotte f(x)}
  LinkGadget(^BoolGad[ 2],"f'(x)  "     ,^Win,160,23); {"      f'(x)}
  LinkGadget(^BoolGad[ 3],"f''(x) "     ,^Win,160,32); {"      f"(x)}
  LinkGadget(^BoolGad[ 4],"f'''(x)"     ,^Win,160,41); {"      f'"(x)}
  LinkGadget(^BoolGad[ 5],"Tangente zu:",^Win,464,50); {"      t(x)}
  LinkGadget(^BoolGad[ 6],"Newton"      ,^Win,520,12); {zeige Newton}
  LinkGadget(^BoolGad[ 7],"Demo"        ,^Win,570,12); {"     "   -Beispiel}
  LinkGadget(^BoolGad[ 8],"Startwert"   ,^Win,520,22); {hole Startwert}
  LinkGadget(^BoolGad[ 9],"Zoom"        ,^Win,  5,42); {Zoom mit Maus}
  LinkGadget(^BoolGad[10],"In"          ,^Win, 55,42); {Zoom mit Konst}
  LinkGadget(^BoolGad[11],"Out"         ,^Win, 75,42); {"    "   "    }
  LinkGadget(^BoolGad[12],"Clear Screen",^Win,160,50); {Koordinatenkreuz}
  LinkGadget(^BoolGad[13],"-10"         ,^Win, 12,20); {kx1}
  LinkGadget(^BoolGad[14]," 10"         ,^Win, 64,20); {kx2}
  LinkGadget(^BoolGad[15],"- 5"         ,^Win, 38,25); {ky1}
  LinkGadget(^BoolGad[16],"  5"         ,^Win, 38,12); {ky2}
  LinkGadget(^BoolGad[17],"---"         ,^Win,560,50); {x}
  LinkGadget(^BoolGad[18],"---"         ,^Win,600,50); {y}
  LinkGadget(^BoolGad[19],"2.5"         ,^Win,100,42); {Zoom}
  LinkGadget(^BoolGad[20],"0.00"        ,^Win,600,22); {Start}
  LinkGadget(^BoolGad[21],"0.000001"    ,^Win,256,50); {Genauigkeit}
  LinkGadget(^BoolGad[22]," 10"         ,^Win,350,50); {Iteration}

  LinkStringGadget(^StrGad,^FunkText,^Undo,^Win,250,81,216,12); {f(x)}
END; (* ErstelleGadgets *)


PROCEDURE ErstelleMenu(MyWindow:Ptr);
BEGIN
  HauptMenu[1]:=Menu(^HauptMenu[2],5,0,8*8,10,1,'Projekt',^MenuPunkt[1],0,0,0,0);
  MenuPunkt[1]:=MenuItem(^MenuPunkt[2],0, 0,14*8,10,ITEMTEXT+HIGHCOMP+ITEMENABLED+COMMSEQ,0,^MenuPunktTxt[1],Nil,'L',Nil,0);
  MenuPunkt[2]:=MenuItem(^MenuPunkt[3],0,10,14*8,10,ITEMTEXT+HIGHCOMP+ITEMENABLED+COMMSEQ,0,^MenuPunktTxt[2],Nil,'S',Nil,0);
  MenuPunkt[3]:=MenuItem(^MenuPunkt[4],0,20,14*8,10,ITEMTEXT+HIGHCOMP+ITEMENABLED        ,0,^MenuPunktTxt[3],Nil,' ',Nil,0);
  MenuPunkt[4]:=MenuItem( Nil         ,0,30,14*8,10,ITEMTEXT+HIGHCOMP+ITEMENABLED+COMMSEQ,0,^MenuPunktTxt[4],Nil,'Q',Nil,0);
  MenuPunktTxt[1]:=IntuiText(1,0,1,2,1,Nil,'Laden',Nil);
  MenuPunktTxt[2]:=IntuiText(1,0,1,2,1,Nil,'Speichern',Nil);
  MenuPunktTxt[3]:=IntuiText(1,0,1,2,1,Nil,'About',Nil);
  MenuPunktTxt[4]:=IntuiText(1,0,1,2,1,Nil,'Ende',Nil);

  HauptMenu[2]:=Menu(^HauptMenu[3],7*8+32,0,7*8,10,1,'Farben',^MenuPunkt[5],0,0,0,0);
  MenuPunkt[ 5]:=MenuItem(^MenuPunkt[ 6],0, 0,21*8,10,ITEMTEXT+HIGHCOMP+ITEMENABLED        ,0,^MenuPunktTxt[ 5],Nil,' ',Nil,0);
  MenuPunkt[ 6]:=MenuItem(^MenuPunkt[ 7],0,10,21*8,10,ITEMTEXT+HIGHCOMP+ITEMENABLED        ,0,^MenuPunktTxt[ 6],Nil,' ',Nil,0);
  MenuPunkt[ 7]:=MenuItem(^MenuPunkt[ 8],0,20,21*8,10,ITEMTEXT+HIGHCOMP+ITEMENABLED        ,0,^MenuPunktTxt[ 7],Nil,' ',Nil,0);
  MenuPunkt[ 8]:=MenuItem(^MenuPunkt[ 9],0,30,21*8,10,ITEMTEXT+HIGHCOMP+ITEMENABLED        ,0,^MenuPunktTxt[ 8],Nil,' ',Nil,0);
  MenuPunkt[ 9]:=MenuItem(^MenuPunkt[10],0,40,21*8,10,ITEMTEXT+HIGHCOMP+ITEMENABLED        ,0,^MenuPunktTxt[ 9],Nil,' ',Nil,0);
  MenuPunkt[10]:=MenuItem(^MenuPunkt[11],0,50,21*8,10,ITEMTEXT+HIGHCOMP+ITEMENABLED        ,0,^MenuPunktTxt[10],Nil,' ',Nil,0);
  MenuPunkt[11]:=MenuItem(^MenuPunkt[12],0,60,21*8,10,ITEMTEXT+HIGHCOMP+ITEMENABLED        ,0,^MenuPunktTxt[11],Nil,' ',Nil,0);
  MenuPunkt[12]:=MenuItem( Nil          ,0,70,21*8,10,ITEMTEXT+HIGHCOMP+ITEMENABLED+COMMSEQ,0,^MenuPunktTxt[12],Nil,'E',Nil,0);
  MenuPunktTxt[ 5]:=IntuiText(1,0,1,2,1,Nil,'Funktion'        ,Nil);
  MenuPunktTxt[ 6]:=IntuiText(1,0,1,2,1,Nil,'1.Ableitung'     ,Nil);
  MenuPunktTxt[ 7]:=IntuiText(1,0,1,2,1,Nil,'2.Ableitung'     ,Nil);
  MenuPunktTxt[ 8]:=IntuiText(1,0,1,2,1,Nil,'3.Ableitung'     ,Nil);
  MenuPunktTxt[ 9]:=IntuiText(1,0,1,2,1,Nil,'Text-/Wertefarbe',Nil);
  MenuPunktTxt[10]:=IntuiText(1,0,1,2,1,Nil,'Tangente(n)'     ,Nil);
  MenuPunktTxt[11]:=IntuiText(1,0,1,2,1,Nil,'Fadenkreuz'      ,Nil);
  MenuPunktTxt[12]:=IntuiText(1,0,1,2,1,Nil,'» Einstellen'    ,Nil);

  HauptMenu[3]:=Menu(^HauptMenu[4],13*8+2*32,0,15*8,10,1,'Werte ausgeben',^MenuPunkt[13],0,0,0,0);
  MenuPunkt[13]:=MenuItem(^MenuPunkt[14],0,  0,24*8,10,ITEMTEXT+HIGHCOMP+ITEMENABLED+COMMSEQ,0,^MenuPunktTxt[13],Nil,'F',Nil,0);
  MenuPunkt[14]:=MenuItem(^MenuPunkt[15],0, 10,24*8,10,ITEMTEXT+HIGHCOMP+ITEMENABLED+COMMSEQ,0,^MenuPunktTxt[14],Nil,'1',Nil,0);
  MenuPunkt[15]:=MenuItem(^MenuPunkt[16],0, 20,24*8,10,ITEMTEXT+HIGHCOMP+ITEMENABLED+COMMSEQ,0,^MenuPunktTxt[15],Nil,'2',Nil,0);
  MenuPunkt[16]:=MenuItem(^MenuPunkt[17],0, 30,24*8,10,ITEMTEXT+HIGHCOMP+ITEMENABLED+COMMSEQ,0,^MenuPunktTxt[16],Nil,'3',Nil,0);
  MenuPunkt[17]:=MenuItem(^MenuPunkt[18],0, 40,24*8,10,ITEMTEXT+HIGHCOMP+ITEMENABLED+COMMSEQ,0,^MenuPunktTxt[17],Nil,'G',Nil,0);
  MenuPunkt[18]:=MenuItem(^MenuPunkt[19],0, 50,24*8,10,ITEMTEXT+HIGHCOMP+ITEMENABLED+COMMSEQ,0,^MenuPunktTxt[18],Nil,'T',Nil,0);
  MenuPunkt[19]:=MenuItem(^MenuPunkt[20],0, 60,24*8,10,ITEMTEXT+HIGHCOMP+ITEMENABLED+COMMSEQ,0,^MenuPunktTxt[19],Nil,'N',Nil,0);
  MenuPunkt[20]:=MenuItem(^MenuPunkt[21],0, 70,24*8,10,ITEMTEXT+HIGHCOMP+ITEMENABLED+COMMSEQ,0,^MenuPunktTxt[20],Nil,'W',Nil,0);
  MenuPunkt[21]:=MenuItem(^MenuPunkt[22],0, 80,24*8,10,ITEMTEXT+HIGHCOMP+ITEMENABLED+COMMSEQ,0,^MenuPunktTxt[21],Nil,'I',Nil,0);
  MenuPunkt[22]:=MenuItem(^MenuPunkt[23],0, 90,24*8,10,ITEMTEXT+HIGHCOMP+ITEMENABLED+COMMSEQ,0,^MenuPunktTxt[22],Nil,'X',Nil,0);
  MenuPunkt[23]:=MenuItem(^MenuPunkt[24],0,100,24*8,10,ITEMTEXT+HIGHCOMP+ITEMENABLED+COMMSEQ,0,^MenuPunktTxt[23],Nil,'Y',Nil,0);
  MenuPunkt[24]:=MenuItem( NIL          ,0,110,24*8,10,ITEMTEXT+HIGHCOMP+ITEMENABLED        ,0,^MenuPunktTxt[24],Nil,' ',Nil,0);
  MenuPunktTxt[13]:=IntuiText(1,0,1,2,1,Nil,'Funktion'           ,Nil);
  MenuPunktTxt[14]:=IntuiText(1,0,1,2,1,Nil,'1.Ableitung'        ,Nil);
  MenuPunktTxt[15]:=IntuiText(1,0,1,2,1,Nil,'2.Ableitung'        ,Nil);
  MenuPunktTxt[16]:=IntuiText(1,0,1,2,1,Nil,'3.Ableitung'        ,Nil);
  MenuPunktTxt[17]:=IntuiText(1,0,1,2,1,Nil,'Intervall-Grenzen'  ,Nil);
  MenuPunktTxt[18]:=IntuiText(1,0,1,2,1,Nil,'Wertetabelle'       ,Nil);
  MenuPunktTxt[19]:=IntuiText(1,0,1,2,1,Nil,'Newton-Wertetabelle',Nil);
  MenuPunktTxt[20]:=IntuiText(1,0,1,2,1,Nil,'Startwert'          ,Nil);
  MenuPunktTxt[21]:=IntuiText(1,0,1,2,1,Nil,'Iteration'          ,Nil);
  MenuPunktTxt[22]:=IntuiText(1,0,1,2,1,Nil,'x-Wert'             ,Nil);
  MenuPunktTxt[23]:=IntuiText(1,0,1,2,1,Nil,'y-Wert'             ,Nil);
  MenuPunktTxt[24]:=IntuiText(1,0,1,2,1,Nil,'Text'               ,Nil);

  HauptMenu[4]:=Menu(^HauptMenu[5],27*8+3*32,0,12*8,10,1,'Preferences',^MenuPunkt[25],0,0,0,0);
  MenuPunkt[25]:=MenuItem(^MenuPunkt[29],0, 0,21*8,10,ITEMTEXT+HIGHCOMP+ITEMENABLED,0,^MenuPunktTxt[25],Nil,' ',^MenuPunkt[26],0);
  MenuPunkt[26]:=MenuItem(^MenuPunkt[27],18*8, 0,13*8,10,ITEMTEXT+HIGHCOMP+ITEMENABLED+CHECKIT+MENUTOGGLE,0,^MenuPunktTxt[26],Nil,' ',Nil,0);
  MenuPunkt[27]:=MenuItem(^MenuPunkt[28],18*8,10,13*8,10,ITEMTEXT+HIGHCOMP+ITEMENABLED+CHECKIT+MENUTOGGLE,0,^MenuPunktTxt[27],Nil,' ',Nil,0);
  MenuPunkt[28]:=MenuItem( NIL          ,18*8,20,13*8,10,ITEMTEXT+HIGHCOMP+ITEMENABLED+CHECKIT+MENUTOGGLE,0,^MenuPunktTxt[28],Nil,' ',Nil,0);
  MenuPunkt[29]:=MenuItem(^MenuPunkt[30],0,10,21*8,10,ITEMTEXT+HIGHCOMP+ITEMENABLED+CHECKIT+MENUTOGGLE,0,^MenuPunktTxt[29],Nil,' ',Nil,0);
  MenuPunkt[30]:=MenuItem( NIL          ,0,20,21*8,10,ITEMTEXT+HIGHCOMP+ITEMENABLED,0,^MenuPunktTxt[30],Nil,' ',^MenuPunkt[31],0);
  MenuPunkt[31]:=MenuItem(^MenuPunkt[32],18*8, 0,10*8,10,ITEMTEXT+HIGHCOMP+ITEMENABLED,0,^MenuPunktTxt[31],Nil,' ',Nil,0);
  MenuPunkt[32]:=MenuItem(^MenuPunkt[33],18*8,10,10*8,10,ITEMTEXT+HIGHCOMP+ITEMENABLED,0,^MenuPunktTxt[32],Nil,' ',Nil,0);
  MenuPunkt[33]:=MenuItem( NIL          ,18*8,20,10*8,10,ITEMTEXT+HIGHCOMP+ITEMENABLED,0,^MenuPunktTxt[33],Nil,' ',Nil,0);
  MenuPunktTxt[25]:=IntuiText(1,0,1,2,1,Nil,'Werte         -»',Nil);
  MenuPunktTxt[26]:=IntuiText(1,0,1,2,1,Nil,'  Drucker'       ,Nil);
  MenuPunktTxt[27]:=IntuiText(1,0,1,2,1,Nil,'  Bildschirm'    ,Nil);
  MenuPunktTxt[28]:=IntuiText(1,0,1,2,1,Nil,'  Grafik'        ,Nil);
  MenuPunktTxt[29]:=IntuiText(1,0,1,2,1,Nil,'  Tangentenschar',Nil);
  MenuPunktTxt[30]:=IntuiText(1,0,1,2,1,Nil,'Konfiguration -»',Nil);
  MenuPunktTxt[31]:=IntuiText(1,0,1,2,1,Nil,'Laden'           ,Nil);
  MenuPunktTxt[32]:=IntuiText(1,0,1,2,1,Nil,'Speichern'       ,Nil);
  MenuPunktTxt[33]:=IntuiText(1,0,1,2,1,Nil,'Default'         ,Nil);

  HauptMenu[5]:=Menu(^HauptMenu[6],38*8+4*32,0,7*8,10,1,'Grafik',^MenuPunkt[34],0,0,0,0);
  MenuPunkt[34]:=MenuItem(^MenuPunkt[35],0, 0,20*8,10,ITEMTEXT+HIGHCOMP+ITEMENABLED+COMMSEQ,0,^MenuPunktTxt[34],Nil,'D',Nil,0);
  MenuPunkt[35]:=MenuItem( NIL          ,0,10,20*8,10,ITEMTEXT+HIGHCOMP+ITEMENABLED        ,0,^MenuPunktTxt[35],Nil,' ',Nil,0);
  MenuPunktTxt[34]:=IntuiText(1,0,1,2,1,Nil,'Drucken'        ,Nil);
  MenuPunktTxt[35]:=IntuiText(1,0,1,2,1,Nil,'Speichern (IFF)',Nil);

  HauptMenu[6]:=Menu(NIL,45*8+5*32,0,10*8,10,1,'Variablen',^MenuPunkt[36],0,0,0,0);
  MenuPunkt[36]:=MenuItem(^MenuPunkt[37],0, 0,80,10,ITEMTEXT+HIGHCOMP+ITEMENABLED+COMMSEQ,0,^MenuPunktTxt[36],Nil,'A',Nil,0);
  MenuPunkt[37]:=MenuItem(^MenuPunkt[38],0,10,80,10,ITEMTEXT+HIGHCOMP+ITEMENABLED+COMMSEQ,0,^MenuPunktTxt[37],Nil,'B',Nil,0);
  MenuPunkt[38]:=MenuItem(^MenuPunkt[39],0,20,80,10,ITEMTEXT+HIGHCOMP+ITEMENABLED+COMMSEQ,0,^MenuPunktTxt[38],Nil,'C',Nil,0);
  MenuPunkt[39]:=MenuItem( NIL          ,0,30,80,10,ITEMTEXT+HIGHCOMP+ITEMENABLED+COMMSEQ,0,^MenuPunktTxt[39],Nil,'D',Nil,0);
  MenuPunktTxt[36]:=IntuiText(1,0,1,2,1,Nil,'  a',Nil);
  MenuPunktTxt[37]:=IntuiText(1,0,1,2,1,Nil,'  b',Nil);
  MenuPunktTxt[38]:=IntuiText(1,0,1,2,1,Nil,'  c',Nil);
  MenuPunktTxt[39]:=IntuiText(1,0,1,2,1,Nil,'  d',Nil);

  SetMenuStrip(MyWindow,^HauptMenu[1])
END; (* ErstelleMenu *)


FUNCTION oeffne_Bildschirm : BOOLEAN;
VAR Scr : NewScreen;
BEGIN
  winptr := NIL;
  scrptr := NIL;
  scrwin := NIL;
  oeffne_Bildschirm:=TRUE;
  Scr:=NewScreen(0,0,s_Breite,s_Hoehe,s_Tiefe,0,1,HIRES,CUSTOMSCREEN,NIL,
                "»» NEWTON-Verfahren zur Ermittlung von Nullstellen «« - © 1991 by TPH",
                NIL,NIL);
  scrptr:=OpenScreen(^Scr);

  IF scrptr = NIL THEN BEGIN
    writeln("- Konnte Screen nicht öffnen");
    oeffne_Bildschirm:=FALSE
  END ELSE BEGIN
    Win:=NewWindow(0,10,s_Breite,s_Hoehe-10,1,0,
                  INACTIVEWINDOW or MOUSEBUTTONS or RAWKEY,
                  BACKDROP or BORDERLESS or RMBTRAP,NIL,NIL,
                  " ... ??? Na los! Ich warte !!!",scrptr,NIL,5,5,
                  s_Breite,s_Hoehe,CUSTOMSCREEN);
    scrwin:=OpenWindow(^Win);
    IF scrwin = NIL THEN BEGIN
      writeln("- Konnte Screen-Fenster nicht öffnen");
      oeffne_Bildschirm:=FALSE;
      CloseScreen(scrptr)
    END ELSE BEGIN
      Win:=NewWindow(0,s_Hoehe-w_Hoehe,w_Breite,w_Hoehe,1,0,
                    GADGETUP or MENUPICK or INACTIVEWINDOW,
                    ACTIVATE or WINDOWDRAG or WINDOWSIZING,NIL,NIL,
                    "Status: Ich bin erwacht !!!",scrptr,NIL,5,5,
                    w_Breite,w_Hoehe,CUSTOMSCREEN);
      ErstelleGadgets;
      winptr:=OpenWindow(^Win);
      IF winptr = NIL THEN BEGIN
        writeln("Konnte Fenster nicht öffnen");
        oeffne_Bildschirm:=FALSE;
        CloseWindow(scrwin);
        CloseScreen(scrptr)
      END (* IF *)
    END (* ELSE *)
  END (* ELSE *)
END; (* oeffne_Bildschirm *)


PROCEDURE schliesse_Bildschirm;
BEGIN
  IF winptr <> NIL THEN CloseWindow(winptr);
  IF scrwin <> NIL THEN CloseWindow(scrwin);
  IF scrptr <> NIL THEN CloseScreen(scrptr)
END; (* schliesse_Bildschirm *)


PROCEDURE oeffne_Libraries;
BEGIN
  OpenLib(IntBase,"intuition.library",0);
  OpenLib(GfxBase,"graphics.library",0);
  OpenLib(DosBase,"dos.library",0);
  OpenLib(ReqBase,"req.library",0);
  InitAnalysis;
END; (* oeffne_Libraries *)


PROCEDURE schliesse_Libraries;
BEGIN
  ExitAnalysis;
  CloseLib(ReqBase);
  CloseLib(DosBase);
  CloseLib(GfxBase);
  CloseLib(IntBase)
END; (* schliesse_Libraries *)


PROCEDURE SetKoordCursor(X,Y:Word);

  PROCEDURE Kreuz(mx,my:Word);
  BEGIN
    SetDrMd(scrwin^.RPort,Complement);
    Move(scrwin^.RPort,mx-1 ,my-8);
    Draw(scrwin^.RPort,mx-1 ,my+8);
    Move(scrwin^.RPort,mx   ,my-8);
    Draw(scrwin^.RPort,mx   ,my+8);
    Move(scrwin^.RPort,mx-16,my  );
    Draw(scrwin^.RPort,mx+16,my  )
  END; (* Kreuz *)

BEGIN
  Kreuz(Faden.X,Faden.Y);
  Faden.X:=X;
  Faden.Y:=Y;
  Kreuz(Faden.X,Faden.Y);
  ZeigXY
END; (* SetKoordCursor *)


PROCEDURE HolVariable(ch:CHAR);
VAR Va    : LONG;
    VaTxt : STRING[80];
    num   : LONG;
BEGIN
  VaTxt:="Nummer für "+ch;
  IF HolZahl(VaTxt,0,99,0,num)=TRUE THEN BEGIN
    VaTxt:="Variable "+ch;
    IF HolZahl(VaTxt,-MaxInt,MaxInt,TRUNC(v[ch,num]),Va)=TRUE THEN
      v[ch,num]:=Va
  END (* IF *)
END; (* HolVariable *)


PROCEDURE Funktionen(c:CHAR); {f(x),g(x),h(x)}
VAR FTxt : Str;               {momentan nur f(x) vom User einstellbar}
BEGIN
  Ein.s:=FunkText;
  Ein.p:=1;
  Inp(ein,fx);
  IF fx<>NIL THEN BEGIN
    Forget(f[c,0]);
    f[c,0]:=fx;
{    Infix(fx,0,FTxt);
    FunkText:=FTxt;}
    RefreshAll
  END (* IF *)
END; (* Funktionen *)


PROCEDURE Plotte(Funk:String;Col:Long);
BEGIN
  Funktionen("f");
  Ein.s:=Funk;
  Ein.p:=1;
  writeln(Ein.s);
  Inp (Ein,fx);
  IF fx<>Nil THEN BEGIN
    Graph (fx,s_Breite,s_Hoehe-10,Col);
    Forget(fx);
  END (* IF *)
END; (* Plotten *)


PROCEDURE Tangente(x0 : ATyp);
BEGIN
  RStr:=RealStr(x0,8);
  WHILE RStr[1]=" " DO Delete(RStr,1,1);
  TempStr:="f'("+RStr+")*x-((f'("+RStr+")*"+RStr+")-f("+RStr+"))";
  Plotte(TempStr,Farbe.Tangente)
END; (* Tangente *)


PROCEDURE WerteTab(Funk:Str);
VAR hi, lo, step : LONG;
    x            : atyp;
    WerteTabelle : STRING[20*80];

  PROCEDURE AddWert(Wert:Str);
  BEGIN
    WerteTabelle:=WerteTabelle+'f('+RealStr(x,0)+') = '+Wert+CHR(10)
  END; (* AddWert *)

BEGIN
  Funktionen("f");
  ein.s:=Funk;
  ein.p:=1;
  WerteTabelle:="";
  Inp(ein,fx);
  IF fx<>NIL THEN BEGIN
    ZeigStatus("Jetzt mußt DU auf die Requester achten!");
    ok:=HolZahl("untere Grenze",-MaxInt,MaxInt,TRUNC(Min.X),lo);
    IF ok=TRUE THEN BEGIN
      ok:=HolZahl("obere Grenze",-MaxInt,MaxInt,TRUNC(Max.X),hi);
      IF hi<lo THEN BEGIN
        step:=hi;
        hi:=lo;
        lo:=step
      END; (* IF *)
      IF hi=lo THEN hi:=hi+TRUNC(Genauigkeit);
      IF ok=TRUE THEN REPEAT
        ok:=HolZahl("Schrittweite",1,MaxInt,1,step);
      UNTIL (step>0) AND (abs(hi-lo)/abs(step)<1e4);
      IF ok=TRUE THEN BEGIN
        i:=0;
        x:=lo;
        IF sgn(hi-lo)=sgn(step) THEN REPEAT
          everr:=false;
          y:=eval(fx,x);
          RStr:=RealStr(eval(fx,x),0);
          IF everr THEN AddWert('?')
                   ELSE AddWert(RStr);
          INC(i);
          x:=lo+i*step
        UNTIL sgn(step)*(x-hi)>1e-5*abs(step);
        Ausgeben(WerteTabelle)
      END (* IF *)
    END; (* IF *)
    Forget(fx)
  END (* IF *)
END; (* WerteTab *)


PROCEDURE Zoomen;
VAR DBox  : BOOLEAN;
    Mitte : Koords;
    Gfx   : Koords;
    h     : Koords;

  PROCEDURE ZeichneBox(Gfx:Koords);
  VAR diff : Koords;
  BEGIN
    diff.X:=abs(Gfx.X-Mitte.X);
    diff.Y:=abs(Gfx.Y-Mitte.Y);
    Move(scrwin^.RPort,TRUNC(Mitte.X-diff.X),TRUNC(Mitte.Y+diff.Y));
    Draw(scrwin^.RPort,TRUNC(Mitte.X-diff.X),TRUNC(Mitte.Y-diff.Y));
    Draw(scrwin^.RPort,TRUNC(Mitte.X+diff.X),TRUNC(Mitte.Y-diff.Y));
    Draw(scrwin^.RPort,TRUNC(Mitte.X+diff.X),TRUNC(Mitte.Y+diff.Y));
    Draw(scrwin^.RPort,TRUNC(Mitte.X-diff.X),TRUNC(Mitte.Y+diff.Y))
  END; (* ZeichneBox *)

BEGIN
  SetDrMd(scrwin^.RPort,COMPLEMENT);
  SetEnde:=FALSE;
  DBox:=FALSE;
  Gfx.X:=0;
  Gfx.Y:=0;
  h.X:=Delta.X;
  h.Y:=Delta.Y;
  REPEAT
    IF Msg2<>NIL THEN Reply_Msg(Msg2);
    IF DBox=TRUE THEN BEGIN
      ZeichneBox(Gfx);
      Gfx.X:=scrwin^.MouseX;
      Gfx.Y:=scrwin^.MouseY;
      ZeichneBox(Gfx)
    END; (* IF *)
    IF (Msg2=NIL) AND (DBox=FALSE) THEN Msg2:=Wait_Port(scrwin^.userPort);
    IF Msg2=NIL THEN Msg2:=p_intuimessage(Get_Msg(scrwin^.userport));
    CASE Msg^.class OF
      MouseButtons: IF DBox=FALSE THEN BEGIN
                      Mitte.X:=scrwin^.MouseX;
                      Mitte.Y:=scrwin^.MouseY;
                      DBox:=TRUE
                    END ELSE SetEnde:=TRUE;
      RawKey      : SetEnde:=TRUE;
    ELSE END (* CASE *)
  UNTIL SetEnde=TRUE;
  IF DBox=TRUE THEN BEGIN
    Min.X:=Umrechnen(Mitte.X-Gfx.X,h.X,forX);
    Max.X:=Umrechnen(Mitte.X+Gfx.X,h.X,forX);
    Min.Y:=Umrechnen(Mitte.Y-Gfx.Y,h.Y,forY);
    Max.Y:=Umrechnen(Mitte.Y+Gfx.Y,h.Y,forY);
    KoordSyst(s_Breite,s_Hoehe-10)
  END (* IF *)
END; (* Zoomen *)


PROCEDURE ZoomIn;
BEGIN
  Min.X:=Min.X+ZoomWert;
  Max.X:=Max.X-ZoomWert;
  Min.Y:=Min.Y+ZoomWert;
  Max.Y:=Max.Y-ZoomWert;
  ControlMinMaxXY;
  KoordSyst(s_Breite,s_Hoehe-10)
END; (* ZoomIn *)


PROCEDURE ZoomOut;
BEGIN
  Min.X:=Min.X-ZoomWert;
  Max.X:=Max.X+ZoomWert;
  Min.Y:=Min.Y-ZoomWert;
  Max.Y:=Max.Y+ZoomWert;
  ControlMinMaxXY;
  KoordSyst(s_Breite,s_Hoehe-10)
END; (* ZoomOut *)


FUNCTION Newton(Wert : atyp) : atyp;
VAR Nwert: atyp;
BEGIN
  Newton:=0;
  ein.s:="x-(f/f')";
  ein.p:=1;
  Inp(ein,fx);
  IF fx<>NIL THEN BEGIN
    everr:=FALSE;
    Nwert:=Eval(fx,Wert);
    IF everr=FALSE THEN Newton:=Nwert;
    Forget(fx)
  END (* IF *)
END; (* Newton *)


PROCEDURE BerechneNewton;
VAR x1,x2,Wert : atyp;
    it         : INTEGER;
    Newtonwerte: STRING[20*80];

PROCEDURE AddNewtonwerte(nw:Str);
BEGIN
  Newtonwerte:=Newtonwerte+RealStr(abs(it-Iteration),0)+") "+nw+CHR(10)
END; (* AddNewtonwerte *)

BEGIN
  Funktionen("f");
  x2:=Startwert;
  it:=Iteration;
  Newtonwerte:="";
  ZeigStatus("Ich berechne 'mal die NEWTON-WERTE !");
  REPEAT
    x1:=x2;
    x2:=Newton(x1);
    RStr:=RealStr(x2,0);
    AddNewtonwerte(RStr);
    IF TanSchar THEN Tangente(x2);
    DEC(it);
  UNTIL (abs(abs(x2)-abs(x1))<Genauigkeit) OR (it=0);
  IF Newtonwerte<>"" THEN Ausgeben(Newtonwerte)
END; (* BerechneNewton *)


PROCEDURE NewtonDemo;
BEGIN
END; (* NewtonDemo *)


PROCEDURE Farbe_fuer(VAR Farb:Long;fuer:Str);
VAR FarbTxt : STRING[80];
BEGIN
  FarbTxt:="Gib das Farbregister für "+fuer+" an!";
  ZeigStatus(FarbTxt);
  Col:=ColorRequest(Farb);
  IF Col<>-1 THEN Farb:=Col
END; (* Farbe_fuer *)


PROCEDURE About;
VAR TMessage : STRING[400];
BEGIN
  ZeigStatus("Beantworte die Frage bitte ehrlich!");
  TMessage:="Ich heiße: »» EVA Newton «« (Meine Schwester heißt 'Eliza')"+CHR(10)+CHR(10)+
    "Das in mir verwandte Newton-Verfahren, dient der Ermittlung von Nullstellen!"+CHR(10)+
    "Ich wurde am 8.10.1991 von Tamio Patrick Honma geschaffen, um im Informatik-"+CHR(10)+
    "Unterricht der Stufe 13.1 des Humboldt-Gymnasiums in Düsseldorf bei Herrn"+CHR(10)+
    "Dr. Kayser zu imponieren!"+CHR(10)+CHR(10)+
    "Bin ich nicht schön? Hm?"+CHR(10);
  Knopf:=TRequest("Ich über mich!",TMessage,"Wow! Bist Du schön!","Geht so ...","Bahh, bist du häßlich!");
  CASE Knopf OF
    0: BEGIN
         TMessage:="Ach 'hau doch ab! Ich kann auch gut ohne dich leben!";
         Knopf:=TRequest("Deine Verurteilung:",TMessage,NIL,NIL,"... Tschüß ... (sniff) ...")
       END; (* BEGIN *)
    1: BEGIN
         TMessage:="Oh! Danke! Ich wußte schon immer, daß wir gut zueinander passen!";
         Knopf:=TRequest("Mein Urteil über deines",TMessage,"Dem stimm' ich voll zu!",NIL,NIL)
       END; (* BEGIN *)
    2: BEGIN
         TMessage:="Wohl nichts GUTES gewohnt, was?"+CHR(10)+
           "Wähl beim nächsten 'mal gefälligst 'Wow! Bist Du schön!'"+CHR(10)+
           "... sonst kannst'e gleich schluß machen!    Ist das jetzt klar !?";
         Knopf:=TRequest("Mein Urteil über deines",TMessage,NIL,"K-K-K-Klar ¡ (schlotter)",NIL)
       END (* BEGIN *)
  ELSE END (* CASE *)
END; (* About *)


PROCEDURE SetzeScreenFarben;
BEGIN
  SetRGB4(^scrptr^.viewport,0, 0, 0, 0);
  SetRGB4(^scrptr^.viewport,1,10,10,10);
  SetRGB4(^scrptr^.viewport,2, 0, 0,15);
  SetRGB4(^scrptr^.viewport,3,15, 7, 0);
  SetRGB4(^scrptr^.viewport,4,15, 0,15);
  SetRGB4(^scrptr^.viewport,5,15,15, 0);
  SetRGB4(^scrptr^.viewport,6, 0,15,15);
  SetRGB4(^scrptr^.viewport,7,15,15,15)
END; (* SetzeScreenFarben *)


PROCEDURE Init_Global_Vars;
BEGIN
  SetzeScreenFarben;
  x := 0+0; { Öffnet mathieeedoubbas.library }
  FunkText:="";
  Min.X := -10;
  Max.X :=  10;
  Min.Y := -5;
  Max.Y :=  5;
  Faden.X:= ROUND(s_Breite/2);
  Faden.Y:= ROUND((s_Hoehe-10)/2);
  Startwert:=0;
  Iteration:=10;
  Genauigkeit:=0.000001;
  ZoomWert:=2.5;
  WITH Farbe DO BEGIN
    Funktion :=1;
    ErsteAbl :=2;
    ZweiteAbl:=3;
    DritteAbl:=4;
    Koord    :=5;
    Tangente :=6;
    WerteF   :=7
  END; (* WITH *)
  Col:=0;

  Reset_FileRequester;
  Verzeichnis:="SYS:Funktionen";
  DateiNam:="Newton.func";

  Ende:=FALSE;

  TanSchar:=FALSE;
  Ausg.Drucker   :=FALSE;
  Ausg.Bildschirm:=FALSE;
  Ausg.Grafik    :=FALSE;

  KoordSyst(s_Breite,s_Hoehe-10);

  Msg :=NIL;
  Msg2:=NIL
END; (* Init_Global_Vars *)


BEGIN  { Hauptprogramm }
  writeln("Abbruch-Gründe von EVA-Newton:");
  oeffne_Libraries;
  IF oeffne_Bildschirm = TRUE THEN BEGIN
    process_ptr  :=p_MyProcess(FindTask(NIL));
    oldwindow_ptr:=process_ptr^.pr_WindowPtr;
    process_ptr^.pr_WindowPtr:=scrwin;

    Init_Global_Vars;

    FOR i:=0 TO 99 DO BEGIN
      FOR c:='f' TO 'h' DO f[c,i]:=Nil;
      FOR c:='a' TO 'd' DO v[c,i]:=0
    END; (* FOR *)

    ErstelleMenu(winptr);
    REPEAT
      ZeigStatus("Alles in Ordnung! (Glaub' ich zumindest)");
      IF Msg<>NIL THEN Reply_Msg(Msg);
      IF Msg=NIL THEN Msg:=Wait_Port(winptr^.userPort);
      Msg:=p_intuimessage(Get_Msg(winptr^.userport));
      Miadr:=Msg^.iaddress;
      CASE Msg^.class OF
        GadgetUp     : CASE Miadr^.GadgetID OF
                          0: RefreshAll;
                          1: Plotte("f"   ,Farbe.Funktion);
                          2: Plotte("f'"  ,Farbe.ErsteAbl);
                          3: Plotte("f''" ,Farbe.ZweiteAbl);
                          4: Plotte("f'''",Farbe.DritteAbl);
                          5: Tangente(Umrechnen(Faden.X,Delta.X,forX));
                          6: BerechneNewton;
                          7: NewtonDemo;
                          8: ErmittleStartwert;
                          9: Zoomen;
                         10: ZoomIn;
                         11: ZoomOut;
                         12: KoordSyst(s_Breite,s_Hoehe-10);
                         13: BEGIN
                               ok:=HolZahl("linke Grenze (Min X):" ,-MaxInt     ,MaxInt,TRUNC(Min.X      ),Wert);
                               IF ok THEN Min.X:=Wert
                             END; (* BEGIN *)
                         14: BEGIN
                               ok:=HolZahl("rechte Grenze (Max X):",-MaxInt     ,MaxInt,TRUNC(Max.X      ),Wert);
                               IF ok THEN Max.X:=Wert
                             END; (* BEGIN *)
                         15: BEGIN
                               ok:=HolZahl("untere Grenze (Min Y):",-MaxInt     ,MaxInt,TRUNC(Min.Y      ),Wert);
                               IF ok THEN Min.Y:=Wert
                             END; (* BEGIN *)
                         16: BEGIN
                               ok:=HolZahl("obere Grenze (Max Y):" ,-MaxInt     ,MaxInt,TRUNC(Max.Y      ),Wert);
                               IF ok THEN Max.Y:=Wert
                             END; (* BEGIN *)
                         17: BEGIN
                               ok:=HolZahl("Fadenkreuz X:"         ,0           ,s_Breite  ,TRUNC(Faden.X),Wert);
                               IF ok THEN Faden.X:=Wert
                             END; (* BEGIN *)
                         18: BEGIN
                               ok:=HolZahl("Fadenkreuz Y:"         ,0           ,s_Hoehe-10,TRUNC(Faden.Y),Wert);
                               IF ok THEN Faden.Y:=Wert
                             END; (* BEGIN *)
                         19: BEGIN
                               ok:=HolZahl("Zoomwert:"             ,0           ,100         ,TRUNC(ZoomWert   ),Wert);
                               IF ok THEN ZoomWert:=Wert
                             END; (* BEGIN *)
                         20: BEGIN
                               ok:=HolZahl("Startwert:"            ,TRUNC(Min.X),TRUNC(Max.X),TRUNC(Startwert  ),Wert);
                               IF ok THEN Startwert:=Wert
                             END; (* BEGIN *)
                         21: BEGIN
                               i:=-2;
                               AWert:=Genauigkeit;
                               REPEAT
                                 INC(i);
                                 AWert:=AWert*10
                               UNTIL AWert>=1;
                               ok:=HolZahl("Nachkommastellen:"          ,1           ,10          ,i            ,Wert);
                               IF ok THEN BEGIN
                                 Genauigkeit:=1;
                                 FOR i:=0 TO Wert DO Genauigkeit:=Genauigkeit/10
                               END (* IF *)
                             END; (* BEGIN *)
                         22: BEGIN
                               ok:=HolZahl("Iteration:"            ,1           ,100   ,Iteration         ,Wert);
                               IF ok THEN Iteration:=Wert
                             END; (* BEGIN *)
                       END; (* CASE *)
        MenuPick     : BEGIN
                         menue  := Msg^.Code and $1f;
                         menitem:=(Msg^.Code and $7e0)  div 32;
                         subitem:=(Msg^.Code and $f800) div 2048;
                         IF Msg^.Code<>$ffff THEN CASE menue OF
                           0: CASE menitem OF
                                0: LadeFunktion;
                                1: SpeichereFunktion;
                                2: About;
                                3: ende:=true
                              END; (* CASE *)
                           1: CASE menitem OF
                                0: Farbe_fuer(Farbe.Funktion ,"f(x)");
                                1: Farbe_fuer(Farbe.ErsteAbl ,"f'(x)");
                                2: Farbe_fuer(Farbe.ZweiteAbl,"f''(x)");
                                3: Farbe_fuer(Farbe.DritteAbl,"f'''(x)");
                                4: Farbe_fuer(Farbe.Koord    ,"das Koordinatenkreuz");
                                5: Farbe_fuer(Farbe.Tangente ,"die Tangente/n");
                                6: Farbe_fuer(Farbe.WerteF   ,"die Werte");
                                7: Col:=ColorRequest(Col)
                              END; (* CASE *)
                           2: CASE menitem OF
                                0: Ausgeben(FunkText);
{                                1: Ausgeben(f1xSTR);
                                2: Ausgeben(f2xSTR);
                                3: Ausgeben(f3xSTR);
}
                                4: BEGIN
                                     Ausgeben(RealStr(Min.X,0)+' - '+RealStr(Max.X,0));
                                     Ausgeben(RealStr(Min.Y,0)+' - '+RealStr(Min.Y,0))
                                   END; (* BEGIN *)
                                5: WerteTab("f");
                                6: BerechneNewton;
                                7: Ausgeben('Startwert für NEWTON: '+RealStr(Startwert,0));
                                8: Ausgeben('Iteration für NEWTON: '+RealStr(Iteration,0));
                                9: Ausgeben('x-Position: '+RealStr(Umrechnen(Faden.X,Delta.X,forX),0));
                               10: Ausgeben('y-Position: '+RealStr(Umrechnen(Faden.Y,Delta.Y,forY),0));
                               11: BEGIN
                                     ok:=GetString(UserText,"Text:",NIL,50,80);
                                     IF ok=TRUE THEN Ausgeben(UserText)
                                   END (* BEGIN *)
                              ELSE END; (* CASE *)
                           3: CASE menitem OF
                                0: CASE subitem OF
                                     0: IF Ausg.Drucker   =TRUE THEN Ausg.Drucker   :=FALSE
                                                                ELSE Ausg.Drucker   :=TRUE;
                                     1: IF Ausg.Bildschirm=TRUE THEN Ausg.Bildschirm:=FALSE
                                                                ELSE Ausg.Bildschirm:=TRUE;
                                     2: IF Ausg.Grafik    =TRUE THEN Ausg.Grafik    :=FALSE
                                                                ELSE Ausg.Grafik    :=TRUE
                                   END; (* CASE *)
                                1: IF TanSchar=TRUE THEN TanSchar:=FALSE
                                                    ELSE TanSchar:=TRUE;
                                2: CASE subitem OF
                                     0: KonfigLaden;
                                     1: KonfigSpeichern;
                                     2: KonfigDefault
                                   END (* CASE *)
                              END; (* CASE *)
                           4: CASE menitem OF
                                0: GrafikDrucken;
                                1: IFFSpeichern
                              END; (* CASE *)
                           5: CASE menitem OF
                                0: HolVariable("a");
                                1: HolVariable("b");
                                2: HolVariable("c");
                                3: HolVariable("d")
                              END (* CASE *)
                         END (* CASE *)
                       END; (* BEGIN *)
{        InactiveWindow:IF IntBase^.ActiveWindow=scrwin THEN BEGIN
                         SetEnde:=FALSE
                         REPEAT
                           IF Msg2<>NIL THEN Reply_Msg(Msg2);
                           IF Msg2=NIL THEN Msg2:=Wait_Port(scrwin^.userPort);
                           Msg2:=p_intuimessage(Get_Msg(scrwin^.userport))
                           CASE Msg2^.class OF
                             MouseButtons: SetKoordCursor(scrwin^.MouseX,scrwin^.MouseY);
                             InactiveWindow,
                             RAWKEY      : SetEnde:=TRUE
                           END (* CASE *)
                         UNTIL SetEnde=TRUE;
                         IF Msg2<>NIL THEN Reply_Msg(Msg2)
                       END (* IF *)
}
      ELSE END (* CASE *)
    UNTIL Ende=TRUE;
    IF Msg<>NIL THEN Reply_Msg(Msg)

  END; (* IF *)

  writeln(CHR(10)+"Ciao! Ciao! Ragazzi !!!");

  FOR i:=0 TO 99 DO FOR c:='f' TO 'h' DO Forget(f[c,i]);
  process_ptr^.pr_WindowPtr:=oldwindow_ptr;
  schliesse_Bildschirm;
  schliesse_Libraries;
END.

{$ link "analysis.o" }

