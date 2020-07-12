unit SimObjects;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type

  TOrderState = (osUntouched,osInSetup,osInProcess,osFinished);
  TWorkcenterStateDef = (wcsWait,wcsSetup,wcsProcess); //wcsBreakdown, wcsBlocked

  TWorkcenterStateProps = record
    State : TWorkcenterStateDef;
    Caption : string;
    Forecolor : TColor;
    Backcolor : TColor;
  end;

  TSimulationDataPoint = record
    Time : longint;
    Value : longint;
  end;

  { TSimulationData }

  TSimulationData = class
  private
    FValueUnit : string;
  public
    Values : Array of TSimulationDataPoint;
    constructor Create;
    function AsString: string;
    function MaxValue : longint;
    function MaxTime : longint;
    function GetNearestIndex(TimePoint : longint) : longint;
    procedure AddValue(Value : longint);
    procedure Reset;
    procedure SaveToFile(filename : string);
  published
    property ValueUnit : string read FValueUnit write FValueUnit;
  end;



var
  WorkcenterStateWait,WorkcenterStateProcess,WorkcenterStateSetup : TWorkcenterStateProps;

function GetWorkcenterStateProps(AState : TWorkcenterStateDef) : TWorkcenterStateProps;

implementation

uses SimController;


function GetWorkcenterStateProps(AState : TWorkcenterStateDef) : TWorkcenterStateProps;
begin
  if AState=wcsSetup then result:=WorkcenterStateSetup
  else if AState=wcsProcess then result:=WorkcenterStateProcess
  else result:=WorkcenterStateWait;
end;

{ TSimulationData }

constructor TSimulationData.Create;
begin
  inherited Create;
  FValueUnit:='';
  Reset;
end;

function TSimulationData.MaxValue: longint;
var
  i,max : longint;
begin
  max:=0;
  for i:=low(Values) to high(Values) do
    if Values[i].Value>max then max:=Values[i].Value;
  result:=max;
end;

function TSimulationData.MaxTime: longint;
begin
  result:=Values[high(Values)].Time;
end;

function TSimulationData.GetNearestIndex(TimePoint: longint): longint;
var
  i : longint;
begin
  for i:=low(Values)+1 to high(Values) do
    if Values[i].Time>TimePoint then
      begin
        result:=Values[i-1].Time;
        exit;
      end;
end;


procedure TSimulationData.AddValue(Value : longint);
begin
  if Values[high(Values)].Value<>Value then
    begin
      if Values[high(Values)].Time<>SimulationController.Time.Time then
        SetLength(Values,Length(Values)+1);
      Values[high(Values)].Value:=Value;
    end;
  Values[high(Values)].Time:=SimulationController.Time.Time
end;

procedure TSimulationData.Reset;
begin
  SetLength(Values,1);
  Values[0].Time:=0;
  Values[0].Value:=0;
end;

procedure TSimulationData.SaveToFile(filename: string);
var
  i : longint;
  sl : TStringList;
begin
  try
    sl:=TStringList.Create;
    for i:=low(Values) to high(Values) do
      sl.Add(IntToStr(Values[i].Time)+';'+IntToStr(Values[i].Value));

    sl.SaveToFile(filename);
  finally
    sl.Free;
  end;
end;

function TSimulationData.AsString : string;
var
  i : longint;
  s : string;
begin
  s:='';
  for i:=low(Values) to high(Values) do
    s:=s+IntToStr(Values[i].Time)+';'+IntToStr(Values[i].Value)+#10;
  result:=s;
end;



initialization

  WorkcenterStateWait.State:=wcsWait;
  WorkcenterStateWait.Caption:='Wait';

  WorkcenterStateSetup.State:=wcsSetup;
  WorkcenterStateSetup.Caption:='Setup';

  WorkcenterStateProcess.State:=wcsProcess;
  WorkcenterStateProcess.Caption:='Process';


finalization



end.

