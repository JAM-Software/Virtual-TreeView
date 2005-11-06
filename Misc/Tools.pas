unit Tools;

//----------------------------------------------------------------------------------------------------------------------
//
// Description:
//   Contains classes and helper routines.
//
//----------------------------------------------------------------------------------------------------------------------
//
// January 2002:
//   Initial implementation.
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  Windows, SysUtils;

type
  TFloatPoint = record
    X, Y: Single;
  end;

  TFloatPoints = array of TFloatPoint;
  
  // TSpline is a simple class to quickly allow spline interpolation given a number of sustain points.
  TSpline = class
  private
    FPoints: TFloatPoints;
    FSpline,
    FHelper: array of Extended;
  public
    constructor Create(const Points: array of TFloatPoint; Reverse: Boolean);

    function Evaluate(X: Extended): Extended;
  end;

//----------------------------------------------------------------------------------------------------------------------

implementation

//----------------------------------------------------------------------------------------------------------------------

constructor TSpline.Create(const Points: array of TFloatPoint; Reverse: Boolean);

// Creates the spline class and initializes the spline array.
// If Reverse is True then X and Y of the source points are exchanged.

var
  D, W: array of Extended;
  I, N: Integer;

begin
  N := Length(Points);
  SetLength(FPoints, N);
  if Reverse then
    for I := 0 to N - 1 do
    begin
      FPoints[I].X :=  Points[I].Y;
      FPoints[I].Y :=  Points[I].X;
    end
  else
    for I := 0 to N - 1 do
    begin
      FPoints[I].X :=  Points[I].X;
      FPoints[I].Y :=  Points[I].Y;
    end;

  SetLength(D, N);
  SetLength(FHelper, N);
  SetLength(W, N);
  SetLength(FSpline, N);

  for I := 1 to N - 2 do
    D[I] := 2 * (FPoints[I + 1].X - FPoints[I - 1].X);
  for I := 0 to N - 2 do
    FHelper[I] := FPoints[I + 1].X - FPoints[I].X;
  for I := 1 to N - 2 do
    W[I] := 6 * ((FPoints[I + 1].Y - FPoints[I].Y) / FHelper[I] - (FPoints[I].Y - FPoints[I - 1].Y) / FHelper[I - 1]);

  FSpline[0] := 0;
  FSpline[N - 1] := 0;
  for I := 1 to N - 2 do
  begin
    W[I + 1] := W[I + 1] - W[I] * FHelper[I] / D[I];
    D[I + 1] := D[I + 1] - FHelper[I] * FHelper[I] / D[I];
  end;
  for I := N - 2 downto 1 do
    FSpline[I] := (W[I] - FHelper[I] * FSpline[I + 1]) / D[I];
end;

//----------------------------------------------------------------------------------------------------------------------

function TSpline.Evaluate(X: Extended): Extended;

  //--------------- local function --------------------------------------------

  function F(X: Extended): Extended;

  begin
    Result := X * X * X - X;
  end;

  //--------------- end local function ----------------------------------------

var
  T: Extended;
  I: Integer;

begin
  I := 0;
  while (I < High(FPoints)) and (X > FPoints[I + 1].X) do
    Inc(I);
  if I < High(FPoints) then
  begin
    T := (X - FPoints[I].X) / FHelper[I];
    Result := T * FPoints[I + 1].Y + (1 - T) * FPoints[I].Y + FHelper[I] * FHelper[I] * (F(T) * FSpline[I + 1] +
      F(1 - T) * FSpline[I]) / 6;
  end
  else
    Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
 
