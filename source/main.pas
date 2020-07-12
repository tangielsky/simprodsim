{
SimProdSim - Simple Production Simulation

A simple simulation of production processes
Uses Microsoft Access database connected via ODBC


EN:
https://techpluscode.de/simprodsim-en/
https://techpluscode.de/production-simulation-with-self-coded-software/

DE:
https://techpluscode.de/simprodsim/
https://techpluscode.de/eigene-simulation-von-produktionsprozessen


(C) 2020 Thomas Angielsky


Version 0.1: first version of the app
             not all functions are implemented, this is a proof of concept
             more information on

Version 0.2: load and save of preferences
             visualization of buffers and state via TSimXYChart component

}

unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, odbcconn, sqldb, db, FileUtil, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, Menus, ActnList, ValEdit, ComCtrls, Buttons, LCLIntf,
  Inifiles,

  SimController,
  SimWorkcenter,
  SimOrder,
  SimArticle,
  SimSource,
  SimSink,
  SimXYChart;


type

  { TMainForm }

  TMainForm = class(TForm)
    ActionObjectUpdate: TAction;
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
    CheckBoxAutoUpdate: TCheckBox;
    ComboboxChart: TComboBox;
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
    PanelWip: TPanel;
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
    Panel23: TPanel;
    PanelChart: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    PanelInfoWork: TPanel;
    PanelInfoWait: TPanel;
    PanelInfoSetup: TPanel;
    PanelLeft: TPanel;
    PanelRight: TPanel;
    PanelSimulation: TPanel;
    PanelSource: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    ScrollBox1: TScrollBox;
    SpeedButton1: TSpeedButton;
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
    Splitter6: TSplitter;
    SQLQuery1: TSQLQuery;
    SQLQuery2: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    Timer1: TTimer;
    Timer2: TTimer;
    TrackBar1: TTrackBar;
    procedure ActionAboutExecute(Sender: TObject);
    procedure ActionCloseExecute(Sender: TObject);
    procedure ActionObjectUpdateExecute(Sender: TObject);
    procedure ActionPreferencesExecute(Sender: TObject);
    procedure ActionSaveStateExecute(Sender: TObject);
    procedure ActionSimResetExecute(Sender: TObject);
    procedure ActionSimStartExecute(Sender: TObject);
    procedure ActionSimStepExecute(Sender: TObject);
    procedure ActionWebExecute(Sender: TObject);
    procedure ComboboxChartChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
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
    SimXYChartObject : TSimXYChart;
    SelectedWorkcenter : TWorkcenter;
    procedure LoadPreferences;
    procedure SavePreferences;
    procedure ApplyPreferences;
    procedure DoNextStep(updatetime: boolean);
    procedure OpenQuery(Query: TSQLQuery; SQL: string);
    procedure ExecQuery(Query: TSQLQuery; SQL: string);
    procedure SetSimulationActive(state: boolean);
    procedure WorkcenterSelected(Sender: TObject);
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
var
  Ini : TInifile;
  s : string;
begin
  FirstStart:=true;

  (*
  try
    Ini:=TInifile.Create(ExtractFilePath(Paramstr(0))+PathDelim+'simprodsim.ini');
    s:='App internal';
    PanelLeft.Width:=Ini.ReadInteger(s,'Left panel',260);
    PanelRight.Width:=Ini.ReadInteger(s,'Right panel',260);
    PanelSource.Height:=Ini.ReadInteger(s,'Source height',260);
    PanelWip.Height:=Ini.ReadInteger(s,'WIP height',260);
    PanelChart.Height:=Ini.ReadInteger(s,'Chart height',150);
  finally
    Ini.Free;
  end;
    *)

  TrackBar1.Position:=1;
  TrackBar1Change(Sender);

  SimulationSource:=TSimulationSource.Create;
  SimulationSource.View:=ListViewSource;
  SimulationController.Source:=SimulationSource;

  SimulationSink:=TSimulationSink.Create;
  SimulationSink.View:=ListViewSink;
  SimulationController.Sink:=SimulationSink;

  SimulationController.ViewWip:=ListViewWip;

  SimXYChartObject:=TSimXYChart.Create(self);
  SimXYChartObject.Align:=alClient;
  PanelChart.InsertControl(SimXYChartObject);

end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if FirstStart then
    begin
      FirstStart:=false;
      LoadPreferences;
      ApplyPreferences;
      ActionSimResetExecute(Sender);
    end;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  Ini : TInifile;
  s : string;
begin
  try
    Ini:=TInifile.Create(ExtractFilePath(Paramstr(0))+PathDelim+'simprodsim.ini');
    s:='App internal';
    Ini.WriteInteger(s,'Left panel',PanelLeft.Width);
    Ini.WriteInteger(s,'Right panel',PanelRight.Width);
    Ini.WriteInteger(s,'Source height',PanelSource.Height);
    Ini.WriteInteger(s,'WIP height',PanelWip.Height);
    Ini.WriteInteger(s,'Chart height',PanelChart.Height);

  finally
    Ini.Free;
  end;
end;



procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ArticleList.Clear;
  OrderList.Clear;
  WorkcenterList.Clear;

  SimulationSink.Free;
  SimulationSource.Free;

  SimXYChartObject.Free;

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

procedure TMainForm.ExecQuery(Query: TSQLQuery; SQL: string);
begin
  Query.Active:=false;
  Query.SQL.Text:=SQL;
  try
    Query.ExecSQL;
    Query.Active:=false;
  except
    MessageDlg('Error on opening query:'+#13#10+SQL,mtWarning,[mbOK],0);
  end;
end;


procedure TMainForm.LoadPreferences;
var
  Ini : TInifile;
  s : string;
begin
  try
    Ini:=TInifile.Create(ExtractFilePath(Paramstr(0))+PathDelim+'simprodsim.ini');
    s:='Database';
    PreferencesForm.EditOdbcName.Text:=Ini.ReadString(s,'ODBC DSN','SimProdSim');

    s:='Desktop';
    PreferencesForm.FileNameEditBackground.Text:=Ini.ReadString(s,'Background image','');
    PreferencesForm.SpinEditBackgroundWidth.Value:=Ini.ReadInteger(s,'Background image width',0);
    PreferencesForm.SpinEditBackgroundHeight.Value:=Ini.ReadInteger(s,'Background image height',0);
    PreferencesForm.ColorButtonBackground.ButtonColor:=StringToColor(Ini.ReadString(s,'Background color',ColorToString(clWhite)));

    s:='Simulation';
    PreferencesForm.ColorButtonProcessB.ButtonColor:=StringToColor(Ini.ReadString(s,'Process background color',ColorToString(clGreen)));
    PreferencesForm.ColorButtonProcessF.ButtonColor:=StringToColor(Ini.ReadString(s,'Process text color',ColorToString(clBlack)));
    PreferencesForm.ColorButtonSetupB.ButtonColor:=StringToColor(Ini.ReadString(s,'Setup background color',ColorToString(clNavy)));
    PreferencesForm.ColorButtonSetupF.ButtonColor:=StringToColor(Ini.ReadString(s,'Setup text color',ColorToString(clWhite)));
    PreferencesForm.ColorButtonWaitB.ButtonColor:=StringToColor(Ini.ReadString(s,'Wait background color',ColorToString(clGray)));
    PreferencesForm.ColorButtonWaitF.ButtonColor:=StringToColor(Ini.ReadString(s,'Wait text color',ColorToString(clBlack)));
    PreferencesForm.SpinEditInterval.Value:=Ini.ReadInteger(s,'Time interval',1);

    s:='App internal';
    PanelLeft.Width:=Ini.ReadInteger(s,'Left panel',260);
    PanelRight.Width:=Ini.ReadInteger(s,'Right panel',260);
    PanelSource.Height:=Ini.ReadInteger(s,'Source height',260);
    PanelWip.Height:=Ini.ReadInteger(s,'WIP height',260);
    PanelChart.Height:=Ini.ReadInteger(s,'Chart height',150);

  finally
    Ini.Free;
  end;
end;

procedure TMainForm.SavePreferences;
var
  Ini : TInifile;
  s : string;
begin
  try
    Ini:=TInifile.Create(ExtractFilePath(Paramstr(0))+PathDelim+'simprodsim.ini');
    s:='Database';
    Ini.WriteString(s,'ODBC DSN',PreferencesForm.EditOdbcName.Text);

    s:='Desktop';
    Ini.WriteString(s,'Background image',PreferencesForm.FileNameEditBackground.Text);
    Ini.WriteInteger(s,'Background image width',PreferencesForm.SpinEditBackgroundWidth.Value);
    Ini.WriteInteger(s,'Background image height',PreferencesForm.SpinEditBackgroundHeight.Value);
    Ini.WriteString(s,'Background color',ColorToString(PreferencesForm.ColorButtonBackground.ButtonColor));

    s:='Simulation';
    Ini.WriteString(s,'Process background color',ColorToString(PreferencesForm.ColorButtonProcessB.ButtonColor));
    Ini.WriteString(s,'Process text color',ColorToString(PreferencesForm.ColorButtonProcessF.ButtonColor));
    Ini.WriteString(s,'Setup background color',ColorToString(PreferencesForm.ColorButtonSetupB.ButtonColor));
    Ini.WriteString(s,'Setup text color',ColorToString(PreferencesForm.ColorButtonSetupF.ButtonColor));
    Ini.WriteString(s,'Wait background color',ColorToString(PreferencesForm.ColorButtonWaitB.ButtonColor));
    Ini.WriteString(s,'Wait text color',ColorToString(PreferencesForm.ColorButtonWaitF.ButtonColor));
    Ini.WriteInteger(s,'Time interval',PreferencesForm.SpinEditInterval.Value);

  finally
    Ini.Free;
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

  Scrollbox1.Color:=PreferencesForm.ColorButtonBackground.ButtonColor;

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

procedure TMainForm.WorkcenterSelected(Sender: TObject);
begin
  if Sender=nil then exit;

  SelectedWorkcenter:=(Sender as TWorkcenter);
  ComboboxChartChange(Sender);
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  DoNextStep(false);
end;

procedure TMainForm.Timer2Timer(Sender: TObject);
begin
  RealTime:=RealTime+1;
  LabelRealTime.Caption:=TSimulationTime.FormatHM(RealTime);

  if CheckBoxAutoUpdate.Checked then
    ActionObjectUpdateExecute(Sender);
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

procedure TMainForm.ActionObjectUpdateExecute(Sender: TObject);
begin
  if SelectedWorkcenter=nil then exit;

  SelectedWorkcenter.UpdateView;
  ComboboxChartChange(Sender);
  SimXYChartObject.Repaint;
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
    begin
      SavePreferences;
      ApplyPreferences;
    end;
end;

procedure TMainForm.ActionSaveStateExecute(Sender: TObject);
var
  i : integer;
  Workcenter : TWorkcenter;
begin
  //Is at the moment used for writing out data of buffers
  for i:=0 to WorkcenterList.Count-1 do
    begin
      Workcenter:=TWorkcenter(WorkcenterList[i]);
      Workcenter.SimDataState.SaveToFile(Workcenter.Id+'-state.txt');
      if Workcenter.InputBuffer.MaxOrders<>0 then
        Workcenter.InputBuffer.SimDataBufferOrders.SaveToFile(Workcenter.Id+'-input.txt');
      if Workcenter.OutputBuffer.MaxOrders<>0 then
        Workcenter.OutputBuffer.SimDataBufferOrders.SaveToFile(Workcenter.Id+'-output.txt');
    end;

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
      Workcenter.OnSelect:=@WorkcenterSelected;
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

procedure TMainForm.ComboboxChartChange(Sender: TObject);
begin
  if SelectedWorkcenter=nil then exit;

  if ComboboxChart.ItemIndex=1 then
    SimXYChartObject.Values:=SelectedWorkcenter.InputBuffer.SimDataBufferOrders
  else if ComboboxChart.ItemIndex=2 then
    SimXYChartObject.Values:=SelectedWorkcenter.OutputBuffer.SimDataBufferOrders
  else SimXYChartObject.Values:=SelectedWorkcenter.SimDataState;
end;



end.

