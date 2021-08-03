Module Module1

    Sub Main()
        CopyDSSRecords("Sample.dss", "CopyTest.dss")
    End Sub


    Private Sub CopyDSSRecords(DSSSource As String, DSSDest As String, Optional Pathname As String = "/GREEN RIVER/GLENFIR/FLOW/01APR1992/1HOUR/OBS/")
        Dim DSSDBSource As New HecDssFile
        Dim DSSDBDest As New HecDssFile
        Dim istat As Integer = 0


        istat = DSSDBSource.ZOPEN(DSSSource)
        istat = DSSDBDest.ZOPEN(DSSDest)

        DSSDBSource.ZSET("MSLVL", "15")


        DSSDBSource.ZSET("PROGRAM", "Test")
        DSSDBDest.ZSET("PROGRAM", "Text")

        DSSDBSource.SuppressErrors = True
        DSSDBDest.SuppressErrors = True

        istat = DSSDBSource.CopyDSSRecord(Pathname, Pathname, DSSDBDest)


        'Dispose records
        DSSDBSource.ZCLOSE()
        DSSDBDest.ZCLOSE()

    End Sub

End Module
