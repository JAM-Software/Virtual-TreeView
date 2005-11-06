Version 1.2.2

Hello,

you can found an optimized version of Zlib 1.2.2 that you can embed into every Delphi executable without use external dlls.
Further I have build a little example hoping to aid "young" Delphi programmers getting in trouble with streams...

Please let me know if you'll discover errors.

These objects and sources are targeted for executing into P6+ CPU core, and Delphi5-6-7-x software.
Benchmarks show this zlib 40% average faster than native DLL distribution and 100% reliable (original test of zlib 1.1.4 version).

Link FastestZlib folder objects to achieve further 20% more performances, but 4% increased files size (thanks to Ray Mond).

How to compress and decompress a file:

procedure TForm1.Button1Click(Sender: TObject);
var
  InputStream, OutputStream: TFileStream;
  DeCompressionStream: TZDecompressionStream;
  CompressionStream: TZCompressionStream;
  InputFileName, OutputFileName: string;
begin
//compress
  InputFileName := 'c:\image.png';
  OutputFilename := 'c:\image.png.bzip';
  InputStream := TFileStream.Create(InputFileName, fmOpenRead);
  OutputStream := TFileStream.Create(OutputFileName, fmCreate);
  CompressionStream := TZCompressionStream.Create(OutputStream, zcFastest);
  CompressionStream.CopyFrom(InputStream, InputStream.Size);
  CompressionStream.Free;
  OutputStream.Free;
  InputStream.Free;

// decompress
  InputFileName := 'c:\image.png.bzip';
  OutputFilename := 'c:\image2.png'; //rename to original into final code
  InputStream := TFileStream.Create(InputFileName, fmOpenRead);
  OutputStream := TFileStream.Create(OutputFileName, fmCreate);
  DecompressionStream := TZDecompressionStream.Create(InputStream);
  OutputStream.CopyFrom(DecompressionStream, 0);
  DecompressionStream.Free;
  OutputStream.Free;
  InputStream.Free;
end;


Cheers,

Roberto Della Pasqua,
www.dellapasqua.com.

Btw: these sources code of zlib 1.2.2 aren't changed by me and now I get some compiler warnings, but don't worry: they are only little hints about code sintax (cross-platform C compiler typical behavior).