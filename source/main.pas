{
SimProdSim - Simple Production Simulation

A simple simulation of production processes
Uses Microsoft Access database connected via ODBC

https://techpluscode.de/simprodsim/
https://techpluscode.de/eigene-simulation-von-produktionsprozessen

(C) 2020 Thomas Angielsky


Version 0.1: first version of the app
             not all functions are implemented, this is a proof of concept
             more information on

}

unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, odbcconn, sqldb, db, FileUtil, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, Menus, ActnList, ValEdit, ComCtrls, Buttons, LCLIntf,

  SimController,
  SimWorkcenter,
  SimOrder,
  SimArticle,
  SimSource,
  SimSink;


type

  { TMainForm }

  TMainForm = class(TForm)
    ActionAbout: TAction;
    ActionSimStep: TAction;
    ActionLoadState: TAction;
    ActionSaveState: TAction;
    ActionWeb: TAction;
    ActionClose: TAction;
    ActionSimStart: TAction;
    ActionSimReset: TAction;
    ActionPreferences: TAction;
    ActionList1: TActionList;
    Datasource1: TDataSource;
    ImageBackground: TImage;
    ImageList32: TImageList;
    ImageList16: TImageList;
    Label2: TLabel;
    Label3: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    LabelTime: TLabel;
    LabelRealTime: TLabel;
    ListViewObject: TListView;
    ListViewSink: TListView;
    ListViewWIP: TListView;
    ListViewSource: TListView;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    N3: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    N2: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    N1: TMenuItem;
    ODBCConnection1: TODBCConnection;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel14: TPanel;
    Panel15: TPanel;
    Panel16: TPanel;
    Panel17: TPanel;
    Panel18: TPanel;
    Panel19: TPanel;
    Panel20: TPanel;
    Panel21: TPanel;
    Panel22: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    PanelInfoWork: TPanel;
    PanelInfoWait: TPanel;
    PanelInfoSetup: TPanel;
    PanelLeft: TPanel;
    PanelRight: TPanel;
    PanelSimulation: TPanel;
    Panel2: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    ScrollBox1: TScrollBox;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButtonStart: TSpeedButton;
    SpeedButton8: TSpeedButton;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Splitter4: TSplitter;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    Timer1: TTimer;
    Timer2: TTimer;
    TrackBar1: TTrackBar;
    procedure ActionAboutExecute(Sender: TObject);
    procedure ActionCloseExecute(Sender: TObject);
    procedure ActionPreferencesExecute(Sender: TObject);
    procedure ActionSimResetExecute(Sender: TObject);
    procedure ActionSimStartExecute(Sender: TObject);
    procedure ActionSimStepExecute(Sender: TObject);
    procedure ActionWebExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    SimulationSource : TSimulationSource;
    SimulationSink : TSimulationSink;
    RealTime : longint;
    FirstStart : boolean;
    procedure ApplyPreferences;
    procedure DoNextStep(updatetime: boolean);
    procedure OpenQuery(Query: TSQLQuery; SQL: string);
    procedure SetSimulationActive(state: boolean);
    { private declarations }
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses SimObjects, SimTime, Preferences;


procedure TMainForm.FormCreate(Sender: TObject);
begin
  FirstStart:=true;

  TrackBar1.Position:=1;
  TrackBar1Change(Sender);

  SimulationSource:=TSimulationSource.Create;
  SimulationSource.View:=ListViewSource;
  SimulationController.Source:=SimulationSource;

  SimulationSink:=TSimulationSink.Create;
  SimulationSink.View:=ListViewSink;
  SimulationController.Sink:=SimulationSink;

  SimulationController.ViewWip:=ListViewWip;

end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ArticleList.Clear;
  OrderList.Clear;
  WorkcenterList.Clear;

  SimulationSink.Free;
  SimulationSource.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if FirstStart then
    begin
      FirstStart:=false;
      ApplyPreferences;
    end;
end;

procedure TMainForm.ApplyPreferences;
begin
  ODBCConnection1.FileDSN:=PreferencesForm.EditOdbcName.Text;
  ODBCConnection1.DatabaseName:=PreferencesForm.EditOdbcName.Text;

  //ImageBackground.Width:=1000;
  //ImageBackground.Height:=1000;
  //ImageBackground.Picture.LoadFromFile('C:\Users\tangi\Documents\Coding\Lazarus\Artikel\ProductionSimulation\images\Layout.bmp');

  WorkcenterStateProcess.Forecolor:=PreferencesForm.ColorButtonProcessF.ButtonColor;
  WorkcenterStateProcess.Backcolor:=PreferencesForm.ColorButtonProcessB.ButtonColor;
  WorkcenterStateSetup.Forecolor:=PreferencesForm.ColorButtonSetupF.ButtonColor;
  WorkcenterStateSetup.Backcolor:=PreferencesForm.ColorButtonSetupB.ButtonColor;
  WorkcenterStateWait.Forecolor:=PreferencesForm.ColorButtonWaitF.ButtonColor;
  WorkcenterStateWait.Backcolor:=PreferencesForm.ColorButtonWaitB.ButtonColor;

  PanelInfoWork.Font.Color:=WorkcenterStateProcess.Forecolor;
  PanelInfoWork.Color:=WorkcenterStateProcess.Backcolor;
  PanelInfoSetup.Font.Color:=WorkcenterStateSetup.Forecolor;
  PanelInfoSetup.Color:=WorkcenterStateSetup.Backcolor;
  PanelInfoWait.Font.Color:=WorkcenterStateWait.Forecolor;
  PanelInfoWait.Color:=WorkcenterStateWait.Backcolor;

  Scrollbox1.Color:=PreferencesForm.ColorButtonDesktop.ButtonColor;

  SimulationController.TimeInterval:=PreferencesForm.SpinEditInterval.Value;
end;

procedure TMainForm.SetSimulationActive(state : boolean);
var Bitmap : TBitmap;
begin
  LabelTime.Caption:=SimulationController.Time.FormatStrHM;

  Timer1.Enabled:=state;
  Timer2.Enabled:=state;

  if state=true then ActionSimStart.ImageIndex:=5
  else ActionSimStart.ImageIndex:=4;

  Bitmap:=TBitmap.Create;
  ImageList32.GetBitmap(ActionSimStart.ImageIndex,Bitmap);
  SpeedButtonStart.Glyph.Assign(Bitmap);
  Bitmap.Free;

  SpeedButtonStart.ImageIndex:=ActionSimStart.ImageIndex;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  DoNextStep(false);
end;

procedure TMainForm.Timer2Timer(Sender: TObject);
begin
  RealTime:=RealTime+1;
  LabelRealTime.Caption:=TSimulationTime.FormatHM(RealTime);
end;

procedure TMainForm.DoNextStep(updatetime : boolean);
begin
  if updatetime or (SimulationController.Time.Second mod 3 =0) then
    LabelTime.Caption:=SimulationController.Time.FormatStrHM;
  SimulationController.DoNextStep;

  if SimulationController.Finished then SetSimulationActive(false);

end;

procedure TMainForm.TrackBar1Change(Sender: TObject);
begin
  Timer1.Interval:=TrackBar1.Position*5;
end;

procedure TMainForm.ActionCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.ActionAboutExecute(Sender: TObject);
begin
  MessageDlg('About','Simple Production Simulation'#13#10#13#10#13#10
    +'A simple simulation of production processes'#13#10#13#10
    +'(C)opyright 2020 by Thomas Angielsky'#13#10
    +'https://techpluscode.de'
    ,mtInformation,[mbOk],0);
end;

procedure TMainForm.ActionPreferencesExecute(Sender: TObject);
begin
  if PreferencesForm.ShowModal=mrOk then
    ApplyPreferences;
end;

procedure TMainForm.ActionSimResetExecute(Sender: TObject);
var
  Workcenter : TWorkcenter;
  Order : TOrder;
  OrderItem : TOrderItem;
  Article : TArticle;
  ListItem : TListItem;
  i,x,y : integer;
  s : string;
begin
  Screen.Cursor:=crHourglass;


  SimulationController.Reset;
  RealTime:=0;

  SimulationSource.Reset;
  SimulationSink.Reset;

  //Articles
  ArticleList.Clear;
  OpenQuery(SQLQuery1,'SELECT * FROM Articles ORDER BY ArticleId');
  SQLQuery1.First;
  while not SQLQuery1.EOF do
    begin
      Article:=TArticle.Create;
      ArticleList.Add(Article);
      Article.Id:=SQLQuery1.FieldByName('ArticleId').AsString;
      Article.Caption:=SQLQuery1.FieldByName('Caption').AsString;
      Article.ImagePath:=SQLQuery1.FieldByName('ImagePath').AsString;
      SQLQuery1.Next;
    end;
  SQLQuery1.Active:=false;

  //Workcenters
  x:=10;
  y:=0;
  WorkcenterList.Clear;
  OpenQuery(SQLQuery1,'SELECT * FROM Workcenters ORDER BY Pos,WorkcenterId');
  SQLQuery1.First;
  while not SQLQuery1.EOF do
    begin
      Workcenter:=TWorkcenter.Create(self);
      WorkcenterList.Add(Workcenter);
      Workcenter.Id:=SQLQuery1.FieldByName('WorkcenterId').AsString;
      Workcenter.Caption:=SQLQuery1.FieldByName('Caption').AsString;
      Workcenter.ImagePath:=SQLQuery1.FieldByName('ImagePath').AsString;
      Workcenter.InputBuffer.MaxOrders:=SQLQuery1.FieldByName('InputOrderMax').AsInteger;
      Workcenter.OutputBuffer.MaxOrders:=SQLQuery1.FieldByName('OutputOrderMax').AsInteger;
      Workcenter.InputBuffer.MinStayInTime:=SQLQuery1.FieldByName('InputMinStayInTime').AsInteger;
      Workcenter.OutputBuffer.MinStayInTime:=SQLQuery1.FieldByName('OutputMinStayInTime').AsInteger;

      Workcenter.View:=ListViewObject;
      Workcenter.Left:=x;
      Workcenter.Top:=10+y;
      Workcenter.State:=wcsSetup;
      Workcenter.State:=wcsWait;
      Scrollbox1.InsertControl(Workcenter);
      x:=x+Workcenter.Width+10;
      if x>Scrollbox1.Width then
        begin
          y:=y+Workcenter.Height+10;
          x:=10;
        end;

      SQLQuery1.Next;
    end;


  //Orders
  OrderList.Clear;
  OpenQuery(SQLQuery1,'SELECT * FROM Orders ORDER BY Pos');
  SQLQuery1.First;
  while not SQLQuery1.EOF do
    begin
      Order:=TOrder.Create;
      OrderList.Add(Order);
      Order.Id:=SQLQuery1.FieldByName('OrderId').AsString;
      Order.ArticleId:=SQLQuery1.FieldByName('ArticleId').AsString;
      Order.Amount:=SQLQuery1.FieldByName('Amount').AsInteger;
      SQLQuery1.Next;
    end;
  SQLQuery1.Active:=false;


  for i:=0 to OrderList.Count-1 do
    begin
      Order:=TOrder(OrderList[i]);
      OpenQuery(SQLQuery1,'SELECT * FROM Workplans WHERE ArticleId='+''''
        +Order.ArticleId+''''+' ORDER BY Pos');
      if SQLQuery1.RecordCount>0 then
        begin
          SQLQuery1.First;
          while not SQLQuery1.EOF do
            begin
              OrderItem:=TOrderItem.Create;
              Order.Add(OrderItem);
              OrderItem.Pos:=SQLQuery1.FieldByName('Pos').AsInteger;
              OrderItem.Amount:=Order.Amount;
              OrderItem.WorkcenterId:=SQLQuery1.FieldByName('WorkcenterId').AsString;
              OrderItem.Setuptime:=Round(SQLQuery1.FieldByName('Setuptime').AsFloat);
              OrderItem.Processtime:=Round(SQLQuery1.FieldByName('Runtime').AsFloat);
              SQLQuery1.Next;
            end;
          SQLQuery1.Active:=false;
        end;

    end;


  //Set orders to sink
  for i:=0 to OrderList.Count-1 do
    begin
      Order:=TOrder(OrderList[i]);
      SimulationController.Source.Buffer.AddOrder(Order);
    end;

  Screen.Cursor:=crDefault;

end;

procedure TMainForm.ActionSimStartExecute(Sender: TObject);
begin
  if ActionSimStart.Tag=0 then SetSimulationActive(true)
  else SetSimulationActive(false);

  if ActionSimStart.Tag=0 then ActionSimStart.Tag:=1
  else ActionSimStart.Tag:=0;
end;

procedure TMainForm.ActionSimStepExecute(Sender: TObject);
begin
  if Timer1.Enabled then SetSimulationActive(false);
  DoNextStep(true);
end;

procedure TMainForm.ActionWebExecute(Sender: TObject);
begin
  OpenURL('https://techpluscode.de/simprodsim');
end;

procedure TMainForm.OpenQuery(Query : TSQLQuery; SQL : string);
begin
  Query.Active:=false;
  Query.SQL.Text:=SQL;
  try
    Query.Open;
  except
    MessageDlg('Error on opening query:'+#13#10+SQL,mtWarning,[mbOK],0);
  end;
end;



end.

