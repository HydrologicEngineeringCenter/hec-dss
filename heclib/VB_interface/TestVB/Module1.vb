

Imports System.IO

Module Module1

    Public Declare Sub TEST_1 Lib "heclib.dll" (ByVal i As Integer)
    Public Declare Sub TEST_2 Lib "heclib.dll" (ByVal filename As String)


    Sub Main()

        hecZset("mlvl", "", 33)
        hecZset("mlvl", "", 33)
        hecZset("DSSV", "", 6)
        TestTimeSeries()
        hecSqueeze("c:\temp\sample9.dss")
        TEST_1(123)
        TEST_2("abc.dss")



    End Sub


    Function TestTimeSeries() As Integer
        Dim ifltab(500) As Integer
        Dim times(1) As Integer
        Dim stat As Integer
        Dim vals(1) As Single
        times(0) = 23456
        times(1) = 23458
        vals(0) = 6.5
        vals(1) = 7.5

        hecZopen(ifltab, "test_vb_interface.dss", 17)
        hecZsits(ifltab, "/languages/vb/zsits//IR-YEAR/v1", times, vals, 123456, "cfs", "PER-AVER", 1, stat)


        Return stat
    End Function


End Module
