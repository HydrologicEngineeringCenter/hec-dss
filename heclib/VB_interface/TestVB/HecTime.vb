Module HecTime
    Public Sub DATJUL(ByVal CDATEin As Date, ByRef JUL As Long, ByRef IERR As Integer)

        '     This subroutine takes a date, in a variety of styles,
        '     and converts it into a julian date in days since Dec 31, 1899.
        '     If no year is provided, the current year is assumed.
        '     If no day is provided, the first of the month is assumed.

        '     Valid style dates include:
        '         March 21, 1982
        '         21 MAR 82
        '         21MAR82
        '         March 21, 1882
        '         March 82  (return julian date for March 21, 1982)
        '         21 March  (return julian date for March 21 of current year)
        '         [Note: March 21 will return julian date for March 1, 1921, not
        '          the 21st of March]
        '         3/21/82  or  3-21-82

        '     See the subroutine YMDDAT for a complete list
        'Converted to VB.NET MJB
        '     CONVERT THE DATE TO YEAR, MONTH, DAY
        Dim IYR As Integer, IMON As Integer, IDAY As Integer
        Try
            IYR = CDATEin.Year
            IMON = CDATEin.Month
            IDAY = CDATEin.Day
        Catch ex As Exception
            IERR = 1250
        End Try

        '     CONVERT THIS FORM INTO JULIAN
        If (IERR = 0) Then
            JUL = IYMDJL(IYR, IMON, IDAY)
        Else
            JUL = -777777
        End If
    End Sub

    'Private Function IYMDJL(ByVal IYR As Integer, ByVal IMON As Integer, ByVal IDAY As Integer) As Integer
    '    '     CONVERT FROM INTEGER IYR IMON IDAY DATE TO DAY COUNT

    '    '     ASSUME AT LEAST 20 BIT INTEGER
    '    '     USE BASE AS  1900 (01 JAN 1900 IS DAY 1)

    '    '     This code includes conversion of a abbrievated 2 digit year
    '    '     to a 4 digit year
    '    'Converted to VB.NET MJB

    '    'Return DateDiff(DateInterval.Day, New DateTime(1900, 1, 1), New DateTime(IYR, IMON, IDAY))
    '    'ToDo: Need to check if this is working right.

    '    Dim NDAY() As Integer = {0, 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334}
    '    Dim IBASE As Integer = 693960
    '    Dim NYR As Integer = IYR
    '    Dim testnum As Integer = -999

    '    'Fix up months, if off
    '    Dim JMON As Integer = IMON
    '    If JMON > 12 Then
    '        NYR = NYR + 1
    '        JMON = JMON - 12
    '    End If
    '    If (JMON < 1) Then
    '        NYR = NYR - 1
    '        JMON = JMON + 12
    '    End If

    '    'Is this an abbrievated year?  Add the century if so.
    '    If NYR < 100 Then ADDCENTURY(NYR)

    '    Dim JYR As Integer = NYR - 1

    '    Dim NLEAP As Integer = JYR / 4 + JYR / 400 - JYR / 100

    '    Dim L1 As Integer = JYR / 4
    '    Dim L2 As Integer = JYR / 400
    '    Dim L3 As Integer = JYR / 100


    '    'Console.WriteLine("JYR / 4: " & L1 & " JYR / 400: " & L2 & " JYR / 100: " & L3)




    '    Dim MLEAP As Integer = 0
    '    If JMON >= 3 Then
    '        If (NYR Mod 4 = 0 And (NYR Mod 100 <> 0 Or (NYR Mod 100 = 0 And NYR Mod 400 = 0))) Then
    '            MLEAP = 1
    '        End If
    '    End If
    '    If ((JMON <= 0) Or (JMON >= 13)) Then
    '        IYMDJL = -777777
    '    Else
    '        IYMDJL = NYR * 365 + NLEAP + NDAY(JMON) + IDAY + MLEAP - IBASE

    '        'Console.WriteLine("2.4:  NYR: " & NYR & " NLEAP: " & NLEAP & " NDAY(JMON): " & NDAY(JMON) & " IDAY: " & IDAY & " MLEAP: " & MLEAP & " IBASE: " & IBASE)

    '        testnum = CInt(DateDiff(DateInterval.Day, New DateTime(1899, 12, 31), New DateTime(IYR, IMON, IDAY)))
    '        If Date.IsLeapYear(IYR) Then
    '            'testdate += 1
    '        End If
    '        'Console.WriteLine(String.Format("IYMDJL Val: {0} = DateDiff Val: {1}: {2}", IYMDJL, testnum, (testnum = IYMDJL).ToString))

    '    End If

    '    'Return IYMDJL
    '    Return testnum


    'End Function

    Private Function IYMDJL(ByVal IYR As Long, ByVal IMON As Long, ByVal IDAY As Long) As Long
        Dim cDateIn As Date
        cDateIn = CDate(IMON & "/" & IDAY & "/" & IYR)
        Return DateDiff("d", "12/31/1899", cDateIn)
    End Function


    Private Sub ADDCENTURY(ByRef IYEAR As Integer)

        '     This takes a two digit year and add the "correct" century
        '     Bill Charley, HEC Oct 1997

        '     A two digit year mean it is the year that falls within the
        '     range of 90 years back (from now) through 10 year forward.

        '    Is it 2 digit?
        'Converted to VB.NET MJB
        If (IYEAR > 100) Then Exit Sub

        '     Get the current century
        Dim IYR As Integer = Now.Year
        Dim IMON As Integer = Now.Month
        Dim IDAY As Integer = Now.Day
        Dim ICENT As Integer = IYR - IYR Mod 100
        '
        '     Add to year, then check if it is within range
        IYEAR = IYEAR + ICENT
        If (IYEAR > (IYR + 10)) Then IYEAR = IYEAR - 100
        If (IYEAR > (IYR - 90)) Then IYEAR = IYEAR + 100

    End Sub

    Sub DATCLL(ByVal JULIN As Long, ByVal INTIME As Long, ByRef JULOUT As Long, ByRef IOUTIM As Long)

        '     DATE CLEAN LARGE

        '     THIS ROUTINE CLEANS UP THE DATE AND OFFSETS BY INCREMENTING
        '     JULIN AND INTIME BY MINS MINUTES SO THAT IOUTIM IS NEVER
        '     GREATER THAN 1440 (THE NUMBER OF MINUTES PER DAY)
        '     The input time (INTIME) is given as an integer*6 or *4 number
        '     The output time is an Integer*3 or *2 number

        'Converted to VB.NET MJB

        'Console.WriteLine("DATCLL   1 - JULOUT: " & JULOUT)

        Dim I1440 As Long, ITEMP As Long
        I1440 = 1440

        If INTIME > I1440 Then
            JULOUT = JULIN + INTIME \ I1440
            ITEMP = INTIME Mod I1440                                        'MLu
            IOUTIM = ITEMP                                                    'MLu
        ElseIf (INTIME < 0) Then
            JULOUT = JULIN + INTIME \ I1440 - 1
            ITEMP = INTIME Mod I1440 + I1440                                'MLu
            IOUTIM = ITEMP                                                    'MLu
        Else
            JULOUT = JULIN
            IOUTIM = INTIME
        End If

        If (IOUTIM = 0) Then
            JULOUT = JULOUT - 1
            IOUTIM = 1440
        End If

        'Console.WriteLine("DATCLL   2 - JULOUT: " & JULOUT)

    End Sub

    Sub JULDAT(ByVal JUL As Long, ByVal ISTYLE As Long, ByRef CDATEout As String, ByRef NDATE As Integer)

        '     Converts an HEC style julian date (days since Dec 31, 1899),
        '     Into a character date of various styles, as shown below
        '
        '     Input:
        '        JUL:  The julian date, in days since Dec 31, 1899
        '     Output:
        '        CDATE:  A character variable to contain the date (should be
        '                long enough to hold the date
        '        NDATE:  The number of characters in CDATE
        '
        '
        '  ISTYLE  Form   ISTYLE   Form      ISTYLE   Form      ISTYLE   Form
        '     LC, 4 CH YR       LC, 2 CH YR     UC, 4 CH YR      UC, 2 CH YR
        '  0  June 2, 1985  10  June 2, 85  100  JUNE 2, 1985  110  JUNE 2, 85
        '  1  Jun 2, 1985   11  Jun 2, 85   101  JUN 2, 1985   111  JUN 2, 85
        '  2  2 June 1985   12  2 June 85   102  2 JUNE 1985   112  2 JUNE 85
        '  3  June 1985     13  June 85     103  JUNE 1985     113  JUNE 85
        '  4  02Jun1985     14  02Jun85     104  02JUN1985     114  02JUN85
        '  5  2Jun1985      15  2Jun85      105  2JUN1985      115  2JUN85
        '  6  Jun1985       16  Jun85       106  JUN1985       116  JUN85
        '  7  02 Jun 1985   17  02 Jun 85   107  02 JUN 1985   117  02 JUN 85
        '  8  2 Jun 1985    18  2 Jun 85    108  2 JUN 1985    118  2 JUN 85
        '  9  Jun 1985      19  Jun 85      109  JUN 1985      119  JUN 85
        '
        '     ISTYLE=-1:  CDATE = 6/2/85       ISTYLE=-11:  CDATE = 06/02/85
        '     ISTYLE=-2:  CDATE = 6-2-85       ISTYLE=-12:  CDATE = 06-02-85
        '
        '     If ISTYLE is zero, it defaults to style 1.
        '
        '
        '
        'Converted to VB.NET MJB
        Dim i As Integer
        Dim IYR As Integer, IMON As Integer, IDAY As Integer
        'Console.WriteLine(JUL)
        'Console.WriteLine("JULDAT   1 - JUL: " & JUL)

        i = JLIYMD(JUL, IYR, IMON, IDAY)
        If (i < 0) Then
            CDATEout = " "
            NDATE = 1
            Exit Sub
        End If
        Dim dattemp As Date
        Try
            dattemp = CDate(String.Format("{0}/{1}/{2}", IMON, IDAY, IYR))
            CDATEout = GetHECDateStyle(dattemp, CInt(ISTYLE))
            NDATE = Len(CDATEout)
        Catch ex As Exception
            CDATEout = " "
            NDATE = 1
        End Try
    End Sub

    Private Function GetHECDateStyle(ByVal datin As Date, ByVal Istyle As Integer) As String
        Dim strRet As String = " "
        Select Case Istyle
            Case 0 : strRet = datin.ToString("MMMM d, yyyy")
            Case 1 : strRet = datin.ToString("MMM d, yyyy")
            Case 2 : strRet = datin.ToString("d MMMM, yyyy")
            Case 3 : strRet = datin.ToString("MMMM yyyy")
            Case 4 : strRet = datin.ToString("ddMMMyyyy")
            Case 5 : strRet = datin.ToString("dMMMyyyy")
            Case 6 : strRet = datin.ToString("MMMyyyy")
            Case 7 : strRet = datin.ToString("dd MMM yyyy")
            Case 8 : strRet = datin.ToString("d MMM yyyy")
            Case 9 : strRet = datin.ToString("MMM yyyy")
            Case 10 : strRet = datin.ToString("MMMM d, yy")
            Case 11 : strRet = datin.ToString("MMM d, yy")
            Case 12 : strRet = datin.ToString("d MMMM, yy")
            Case 13 : strRet = datin.ToString("MMMM yy")
            Case 14 : strRet = datin.ToString("ddMMMyy")
            Case 15 : strRet = datin.ToString("dMMMyy")
            Case 16 : strRet = datin.ToString("MMMyy")
            Case 17 : strRet = datin.ToString("dd MMM yy")
            Case 18 : strRet = datin.ToString("d MMM yy")
            Case 19 : strRet = datin.ToString("MMM yy")
            Case 100 : strRet = datin.ToString("MMMM d, yyyy")
            Case 101 : strRet = datin.ToString("MMM d, yyyy")
            Case 102 : strRet = datin.ToString("d MMMM, yyyy")
            Case 103 : strRet = datin.ToString("MMMM yyyy")
            Case 104 : strRet = datin.ToString("ddMMMyyyy")
            Case 105 : strRet = datin.ToString("dMMMyyyy")
            Case 106 : strRet = datin.ToString("MMMyyyy")
            Case 107 : strRet = datin.ToString("dd MMM yyyy")
            Case 108 : strRet = datin.ToString("d MMM yyyy")
            Case 109 : strRet = datin.ToString("MMM yyyy")
            Case 110 : strRet = datin.ToString("MMMM d, yy")
            Case 111 : strRet = datin.ToString("MMM d, yy")
            Case 112 : strRet = datin.ToString("d MMMM, yy")
            Case 113 : strRet = datin.ToString("MMMM yy")
            Case 114 : strRet = datin.ToString("ddMMMyy")
            Case 115 : strRet = datin.ToString("dMMMyy")
            Case 116 : strRet = datin.ToString("MMMyy")
            Case 117 : strRet = datin.ToString("dd MMM yy")
            Case 118 : strRet = datin.ToString("d MMM yy")
            Case 119 : strRet = datin.ToString("MMM yy")
        End Select
        If Istyle > 99 Then
            strRet = strRet.ToUpper
        End If
        Return strRet
    End Function

    Private Function JLIYMD(ByVal JUL As Long, ByRef IYR As Integer, ByRef IMON As Integer, ByRef IDAY As Integer) As Integer

        Dim IBASE As Long = 693960
        JLIYMD = 0
        Dim JUL2 As Long
        Dim JULB As Long
        Dim blnFlag As Boolean = False
        '
        Dim JULC As Long = JUL + IBASE
        'IYR = FLOAT(JULC) / 365.2425


        Dim testIYR As Decimal

        'Console.WriteLine("  1:  JULC: " & JULC)
        'Console.WriteLine("  1:  JUL: " & JUL & " JUL2: " & JUL2 & " JULB: " & JULB & " IYR: " & IYR & " IMON: " & IMON & " IDAY: " & IDAY)
        testIYR = CDec(JULC) / 365.2425

        'Console.WriteLine("  2:  testIYR: " & testIYR)
        IYR = CDec(JULC) / 365.2425

        'Console.WriteLine("  2:  JUL: " & JUL & " JUL2: " & JUL2 & " JULB: " & JULB & " IYR: " & IYR & " IMON: " & IMON & " IDAY: " & IDAY)

        If IYR < 0 Then
            SetBadDate(JLIYMD, IYR, IMON, IDAY)
        Else
            For i As Integer = 1 To 20
                'Console.WriteLine("2.3:  JUL: " & JUL & " JUL2: " & JUL2 & " JULB: " & JULB & " IYR: " & IYR & " IMON: " & IMON & " IDAY: " & IDAY)
                JUL2 = IYMDJL(IYR, 1, 1)
                'Console.WriteLine("2.5:  JUL: " & JUL & " JUL2: " & JUL2 & " JULB: " & JULB & " IYR: " & IYR & " IMON: " & IMON & " IDAY: " & IDAY)
                If (JUL2 > JUL) Then
                    blnFlag = True
                    Exit For
                End If
                IYR = IYR + 1
            Next


            'Console.WriteLine("  3:  JUL: " & JUL & " JUL2: " & JUL2 & " JULB: " & JULB & " IYR: " & IYR & " IMON: " & IMON & " IDAY: " & IDAY)
            If Not blnFlag Then
                SetBadDate(JLIYMD, IYR, IMON, IDAY)
                'Console.WriteLine("  4:  JUL: " & JUL & " JUL2: " & JUL2 & " JULB: " & JULB & " IYR: " & IYR & " IMON: " & IMON & " IDAY: " & IDAY)
            Else
                IYR = IYR - 1
                'Console.WriteLine("  5:  JUL: " & JUL & " JUL2: " & JUL2 & " JULB: " & JULB & " IYR: " & IYR & " IMON: " & IMON & " IDAY: " & IDAY)
                JULB = IYMDJL(IYR, 1, 1)
                'Console.WriteLine("  6:  JUL: " & JUL & " JUL2: " & JUL2 & " JULB: " & JULB & " IYR: " & IYR & " IMON: " & IMON & " IDAY: " & IDAY)
                Dim tempval As Integer = CInt((JUL - JULB + 28) \ 28)
                Dim JA As Integer

                
                If tempval > 12 Then
                    JA = 12
                Else
                    JA = tempval
                End If

                'Console.WriteLine("tempval: " & tempval & " JA: " & JA)

                blnFlag = False
                For IMON = JA To 12
                    'Console.WriteLine("  7:  JUL: " & JUL & " JUL2: " & JUL2 & " JULB: " & JULB & " IYR: " & IYR & " IMON: " & IMON & " IDAY: " & IDAY)
                    JUL2 = IYMDJL(IYR, IMON, 1)
                    'Console.WriteLine("  8:  JUL: " & JUL & " JUL2: " & JUL2 & " JULB: " & JULB & " IYR: " & IYR & " IMON: " & IMON & " IDAY: " & IDAY)
                    If (JUL2 > JUL) Then
                        blnFlag = True
                        Exit For
                    End If
                Next

                If Not blnFlag Then
                    IMON = 13
                    'Console.WriteLine("  9:  JUL: " & JUL & " JUL2: " & JUL2 & " JULB: " & JULB & " IYR: " & IYR & " IMON: " & IMON & " IDAY: " & IDAY)
                End If

                IMON = IMON - 1
                'Console.WriteLine(" 10:  JUL: " & JUL & " JUL2: " & JUL2 & " JULB: " & JULB & " IYR: " & IYR & " IMON: " & IMON & " IDAY: " & IDAY)
                JUL2 = IYMDJL(IYR, IMON, 1)
                'Console.WriteLine(" 11:  JUL: " & JUL & " JUL2: " & JUL2 & " JULB: " & JULB & " IYR: " & IYR & " IMON: " & IMON & " IDAY: " & IDAY)
                IDAY = CInt(JUL - JUL2 + 1)


                'Console.WriteLine(" 12:  JUL: " & JUL & " JUL2: " & JUL2 & " JULB: " & JULB & " IYR: " & IYR & " IMON: " & IMON & " IDAY: " & IDAY)

                If IDAY = 32 Then
                    Console.WriteLine("IDAY was 32")
                    IDAY -= 1
                End If

            End If
        End If
        Return JLIYMD
    End Function

    Private Sub SetBadDate(ByRef JLIYMD As Integer, ByRef IYR As Integer, ByRef IMON As Integer, ByRef IDAY As Integer)
        JLIYMD = -1
        IYR = 0
        IMON = 0
        IDAY = 0
    End Sub
End Module
