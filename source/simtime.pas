unit SimTime;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  SimObjects;

type
  TSimulationTime = class
  private
    FSecond : longint;
  public
    constructor Create;
    destructor Destroy;
    class function FormatHM(value : longint) : string; static;
    function FormatStr : string;
    function FormatStrHM: string;
    function FormatStrMS: string;
    procedure Reset;
    procedure Inc;
    procedure Assign(Time : TSimulationTime);
  published
    property Second : longint read FSecond write FSecond;
    property Time : longint read FSecond;
  end;


implementation


constructor TSimulationTime.Create;
begin
  Reset;
end;

destructor TSimulationTime.Destroy;
begin

end;

class function TSimulationTime.FormatHM(value : longint) : string;
var
  h,min,sek,l,day : longint;
begin
  day:=value div 86400; // full days
  l:=value-day*86400;
  h:=l div 3600;
  l:=l-h*3600;
  min:=l div 60;
  sek:=l-min*60;
  result:=IntToStr(day)+' d '+IntToStr(h)+' h '+IntToStr(min)+' min '
    +IntToStr(sek)+' s';
end;

function TSimulationTime.FormatStr: string;
var
  h,min,sek,l,day : longint;
begin
  day:=Fsecond div 86400; // full days
  l:=FSecond-day*86400;
  min:=l div 60; // full minutes
  sek:=l-min*60;
  result:=IntToStr(day)+' d '+IntToStr(min)+' min '+IntToStr(sek)+' s';
end;

function TSimulationTime.FormatStrHM: string;
begin
  result:=FormatHM(FSecond);
end;

function TSimulationTime.FormatStrMS: string;
var
  min : longint;
  sek : longint;
begin
  min:=FSecond div 60;
  sek:=FSecond-min*60;
  result:=IntToStr(min)+' min '+IntToStr(sek)+' s';
end;

procedure TSimulationTime.Reset;
begin
  FSecond:=0;
end;

procedure TSimulationTime.Inc;
begin
  FSecond:=FSecond+1;
end;

procedure TSimulationTime.Assign(Time: TSimulationTime);
begin
  FSecond:=Time.Second;
end;




end.

