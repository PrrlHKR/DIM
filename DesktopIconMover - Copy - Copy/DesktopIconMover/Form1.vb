Public Class Form1
    Dim p() As Point
    Private Declare Auto Function FindWindow Lib "user32" (ByVal lpClassName As String, ByVal lpWindowName As String) As Int32

    Declare Function FindWindowEx Lib "user32.dll" Alias "FindWindowExA" ( _
    ByVal hWnd1 As Int32, _
    ByVal hWnd2 As Int32, _
    ByVal lpsz1 As String, _
    ByVal lpsz2 As String) As Int32

    Private Declare Auto Function SendMessage Lib "user32" (ByVal hwnd As Int32, ByVal wMsg As Int32, ByVal wParam As Int32, ByVal lParam As Int32) As Int32

#Region "MODES"
    'Auto Arrange off, Align to Grid off:
    '"FFlags"=dword:40200220

    'Auto Arrange on:
    '"FFlags"=dword:40200221

    'Auto Arrange off - align to Grid On:
    '"FFlags"=dword:40200224

    'Auto Arange on, Align to Grid on:
    '"FFlags"=dword40200225

    'Hide Desktop Icons:
    '"FFlags"=dword:40201220

    Sub test()
        Dim b() As Byte = My.Computer.Registry.GetValue("HKEY_CURRENT_USER\Software\Microsoft\Windows\Shell\Bags\1\Desktop", "sort", "0")
        Dim s As String = ""
        For i = 0 To b.Length - 1
            s &= b(i)
        Next
        MsgBox(s)
        Clipboard.SetText(s)
    End Sub

    'Sort by name:
    '"Sort"=dword:00000000

    'Sort by size:
    '"Sort"=dword:00000001

    'Sort by type:
    '"Sort"=dword:00000002

    'Sort by Modified:
    '"Sort"=dword:00000003
#End Region

    'Everywhere you see Int32 you are going to replace with "Long". If that doesn't work, try just using Integer. The hard part of this whole thing is the data types. I tried this out on VB.NET 2005 with winXP and it took me awhile to get these headers down straight since the documentation for the API calls are poor.

    'Once you have those in place, you can use the code below...

    Private Const LVM_GETTITEMCOUNT& = (&H1000 + 4)
    Private Const LVM_SETITEMPOSITION& = (&H1000 + 15)

    Dim hdesk, icount As Integer

    Public Sub MoveIcons()
        hdesk = FindWindow("progman", vbNullString)
        hdesk = FindWindowEx(hdesk, 0, "SHELLDLL_DefView", vbNullString)
        hdesk = FindWindowEx(hdesk, 0, "SysListView32", vbNullString)
        'hdesk is the handle of the Desktop's syslistview32

        icount = SendMessage(hdesk, LVM_GETTITEMCOUNT, 0, 0)
        '0 is "My Computer"
        'X = 400 : Y = 400 'set the position parameters in pixel

        ReDim gpt(icount)
        ReDim p(icount)
    End Sub
    Sub setIconPos(ByVal index As Integer, ByVal x As Integer, ByVal y As Integer)
        p(index) = New Point(x, y)
        Call SendMessage(hdesk, LVM_SETITEMPOSITION, index, Convert.ToInt32(x + y * &H10000))
    End Sub

    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        MoveIcons()
    End Sub
    Dim rnd As New Random
    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        'test()
        'setIconPos(0, 400, 400)
    End Sub
    Sub clock()
        Dim x As Integer = Screen.PrimaryScreen.Bounds.Width / 2 - 32
        Dim y As Integer = Screen.PrimaryScreen.Bounds.Height / 2 - 64
        Dim hig As Integer = y
        Dim wid As Integer = x

        'USE THIS STATEMENT IF YOU WANT A TRUE CIRCULAR CLOCK
        wid = hig

        Dim st As Integer = Math.Floor((icount - 12) / 3)

        For i = 0 To st
            Dim rx As Integer = wid * ((i - 0) / st)
            Dim ry As Integer = hig * ((i - 0) / st)

            Dim a As Single = ((Date.Now.Second + Date.Now.Millisecond / 1000) / 60) * 2 * Math.PI
            a -= 2 * Math.PI
            a -= Math.PI / 2

            Dim xx As Single = Math.Cos(a) * rx + x
            Dim yy As Single = Math.Sin(a) * ry + y
            gpt(i) = New Point(xx, yy)
        Next

        For i = st + 1 To st * 2
            Dim rx As Integer = wid * ((i - (st + 1)) / (st * 2 - (st + 1))) * 0.75
            Dim ry As Integer = hig * ((i - (st + 1)) / (st * 2 - (st + 1))) * 0.75


            Dim a As Single = ((Date.Now.Minute + Date.Now.Second / 60) / 60) * 2 * Math.PI
            a -= 2 * Math.PI
            a -= Math.PI / 2

            Dim xx As Single = Math.Cos(a) * rx + x
            Dim yy As Single = Math.Sin(a) * ry + y
            gpt(i) = New Point(xx, yy)
        Next

        For i = st * 2 + 1 To icount - 13
            Dim rx As Integer = wid * ((i - (st * 2 + 1)) / (icount - 13 - (st * 2 + 1))) * 0.5
            Dim ry As Integer = hig * ((i - (st * 2 + 1)) / (icount - 13 - (st * 2 + 1))) * 0.5


            Dim a As Single = ((Date.Now.Hour + Date.Now.Minute / 60) / 12) * 2 * Math.PI
            a -= 2 * Math.PI
            a -= Math.PI / 2

            Dim xx As Single = Math.Cos(a) * rx + x
            Dim yy As Single = Math.Sin(a) * ry + y
            gpt(i) = New Point(xx, yy)
        Next

        For i = icount - 12 To icount
            Dim a As Single = ((i - (icount - 12)) / (icount - (icount - 12))) * 2 * Math.PI

            Dim xx As Single = Math.Cos(a) * wid + x
            Dim yy As Single = Math.Sin(a) * hig + y
            gpt(i) = New Point(xx, yy)
        Next
    End Sub
    Dim vel As Integer = 100
    Dim v As Integer = 100
    Dim l As Boolean = False
    Dim l2 As Integer = 0
    Dim spacey As Integer = 98 'Icon height spacing
    Dim spacex As Integer = 75 'Icon width spacing
    Private Sub Timer1_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer1.Tick
        Timer1.Enabled = False
        Me.Visible = False
        Dim x As Integer = Screen.PrimaryScreen.Bounds.Width / 2
        Dim y As Integer = Screen.PrimaryScreen.Bounds.Height / 2
        If (Cursor.Position.X < 100) OrElse l = True Then
            l = True
            Dim x2 As Integer = 13
            Dim y2 As Integer = 2
            ReDim Preserve vl(gpt.Length - 1)
            Dim m As Single = 0
            For i = 0 To icount
                If y2 + spacey > (y * 2) - 40 Then
                    y2 = 2
                    x2 += spacex
                End If
                gpt(i) = New Point(x2, y2)
                y2 += spacey
                vl(i) = Math.Sqrt((gpt(i).X - p(i).X) ^ 2 + (gpt(i).Y - p(i).Y) ^ 2)
                If vl(i) > m Then
                    m = vl(i)
                End If
            Next
            m = v / m
            l2 = x2 + spacex
            Dim b As Boolean = True
            For i = 0 To icount
                'Dim vel As Single = vl(i) * m
                If p(i).X > gpt(i).X + (vel / 2) Then
                    p(i) = New Point(p(i).X - vel, p(i).Y)
                    b = False
                ElseIf p(i).X < gpt(i).X - (vel / 2) Then
                    p(i) = New Point(p(i).X + vel, p(i).Y)
                    b = False
                Else
                    p(i) = gpt(i)
                End If
                If p(i).Y > gpt(i).Y + (vel / 2) Then
                    p(i) = New Point(p(i).X, p(i).Y - vel)
                    b = False
                ElseIf p(i).Y < gpt(i).Y - (vel / 2) Then
                    p(i) = New Point(p(i).X, p(i).Y + vel)
                    b = False
                Else
                    p(i) = gpt(i)
                End If
            Next
            If b Then
                System.Threading.Thread.Sleep(1000)
                If Cursor.Position.X > l2 Then
                    l = False
                End If
                '                    MsgBox(l)
            End If
        Else
            l = False
            clock()
            ReDim Preserve vl(p.Length - 1)
            Dim m As Integer = 0
            For i = 0 To p.Length - 1
                vl(i) = Math.Sqrt((gpt(i).X - p(i).X) ^ 2 + (gpt(i).Y - p(i).Y) ^ 2)
                If vl(i) > m Then
                    m = vl(i)
                End If
            Next
            If m <> 0 Then
                m = v / m
            End If
            For i = 0 To icount
                Dim b As Boolean = True
                'Dim vel As Single = vl(i) * m
                If vel = 0 Then
                    MsgBox(0)
                End If
                If p(i).X > gpt(i).X + (vel / 2) Then
                    p(i) = New Point(p(i).X - vel, p(i).Y)
                ElseIf p(i).X < gpt(i).X - (vel / 2) Then
                    p(i) = New Point(p(i).X + vel, p(i).Y)
                Else
                    p(i) = gpt(i)
                    b = False
                End If
                If p(i).Y > gpt(i).Y + (vel / 2) Then
                    p(i) = New Point(p(i).X, p(i).Y - vel)
                ElseIf p(i).Y < gpt(i).Y - (vel / 2) Then
                    p(i) = New Point(p(i).X, p(i).Y + vel)
                Else
                    p(i) = gpt(i)
                    b = False
                End If
                If b Then
                    If Cursor.Position.X <= l2 Then
                        l = True
                    End If
                End If
            Next
            End If
            For i = 0 To icount
                setIconPos(i, p(i).X, p(i).Y)
            Next

            Me.Text = l
            Timer1.Enabled = True
    End Sub
    Dim gpt() As Point
    Dim vl() As Single

    Private Sub QuitToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles QuitToolStripMenuItem.Click
        Timer1.Enabled = False

        NotifyIcon1.Visible = False

        Dim x2 As Integer = 13
        Dim y2 As Integer = 2

        For i = 0 To icount
            If y2 + spacey > Screen.PrimaryScreen.Bounds.Height - 40 Then
                y2 = 2
                x2 += spacex
            End If
            setIconPos(i, x2, y2)
            y2 += spacey
        Next

        End
    End Sub
End Class
