Imports System
Imports System.Drawing
Imports System.Windows.Forms
Imports System.Collections.Generic
Imports System.Linq
Imports System.Diagnostics

Public Class MinecraftClone
    Inherits Form

    ' Game Constants
    Private Const WORLD_SIZE As Integer = 64
    Private Const CHUNK_SIZE As Integer = 16
    Private Const WATER_LEVEL As Integer = 32
    Private Const BLOCK_SIZE As Integer = 20
    Private Const RENDER_DISTANCE As Integer = 8

    ' Block Types
    Private Enum BlockType
        Air = 0
        Grass = 1
        Dirt = 2
        Stone = 3
        Sand = 4
        Water = 5
        Wood = 6
        Leaves = 7
        Bedrock = 8
        CoalOre = 9
        IronOre = 10
    End Enum

    ' Block Colors and Properties
    Private ReadOnly blockColors As Dictionary(Of BlockType, Color) = New Dictionary(Of BlockType, Color) From {
        {BlockType.Air, Color.Transparent},
        {BlockType.Grass, Color.FromArgb(86, 125, 70)},
        {BlockType.Dirt, Color.FromArgb(121, 85, 58)},
        {BlockType.Stone, Color.FromArgb(128, 128, 128)},
        {BlockType.Sand, Color.FromArgb(218, 210, 158)},
        {BlockType.Water, Color.FromArgb(64, 164, 223)},
        {BlockType.Wood, Color.FromArgb(102, 76, 48)},
        {BlockType.Leaves, Color.FromArgb(96, 161, 123)},
        {BlockType.Bedrock, Color.FromArgb(115, 115, 115)},
        {BlockType.CoalOre, Color.FromArgb(64, 64, 64)},
        {BlockType.IronOre, Color.FromArgb(216, 175, 147)}
    }

    Private ReadOnly blockTextures As Dictionary(Of BlockType, Brush) = New Dictionary(Of BlockType, Brush)
    Private ReadOnly transparentBlocks As HashSet(Of BlockType) = New HashSet(Of BlockType) From {BlockType.Air, BlockType.Water, BlockType.Leaves}
    Private ReadOnly solidBlocks As HashSet(Of BlockType) = New HashSet(Of BlockType) From {
        BlockType.Grass, BlockType.Dirt, BlockType.Stone, BlockType.Sand,
        BlockType.Wood, BlockType.Bedrock, BlockType.CoalOre, BlockType.IronOre
    }

    ' Game World
    Private world(,,) As BlockType
    Private noise As New PerlinNoise()
    Private random As New Random()

    ' Player
    Private playerPosition As New Vector3(WORLD_SIZE / 2, 80, WORLD_SIZE / 2)
    Private playerVelocity As New Vector3(0, 0, 0)
    Private playerRotation As New Vector2(0, 0)
    Private playerOnGround As Boolean = False
    Private playerHealth As Integer = 20
    Private playerHunger As Integer = 20
    Private playerInventory As New Dictionary(Of BlockType, Integer)
    Private selectedBlock As BlockType = BlockType.Grass

    ' Rendering
    Private gameTimer As New Timer()
    Private renderBuffer As Bitmap
    Private graphicsBuffer As Graphics
    Private camera As New Camera()
    Private fpsCounter As Integer = 0
    Private fpsTimer As New Stopwatch()
    Private lastFps As Integer = 0

    ' Input
    Private keyState As New HashSet(Of Keys)()
    Private mouseDown As Boolean = False
    Private mousePosition As Point

    ' UI
    Private hotbarSlots As Integer = 9
    Private hotbarSelection As Integer = 0

    Public Sub New()
        InitializeComponent()
        InitializeGame()
        InitializeWorld()
    End Sub

    Private Sub InitializeComponent()
        Me.Text = "Minecraft VB.NET Clone"
        Me.Size = New Size(1200, 800)
        Me.StartPosition = FormStartPosition.CenterScreen
        Me.DoubleBuffered = True
        Me.KeyPreview = True

        ' Setup game timer
        gameTimer.Interval = 16
        AddHandler gameTimer.Tick, AddressOf GameLoop

        ' Setup event handlers
        AddHandler Me.KeyDown, AddressOf OnKeyDown
        AddHandler Me.KeyUp, AddressOf OnKeyUp
        AddHandler Me.MouseDown, AddressOf OnMouseDown
        AddHandler Me.MouseUp, AddressOf OnMouseUp
        AddHandler Me.MouseMove, AddressOf OnMouseMove
        AddHandler Me.MouseWheel, AddressOf OnMouseWheel

        ' Initialize render buffer
        renderBuffer = New Bitmap(Me.ClientSize.Width, Me.ClientSize.Height)
        graphicsBuffer = Graphics.FromImage(renderBuffer)

        ' Start game loop
        gameTimer.Start()
        fpsTimer.Start()
    End Sub

    Private Sub InitializeGame()
        ' Initialize player inventory
        For Each blockType As BlockType In [Enum].GetValues(GetType(BlockType))
            If blockType <> BlockType.Air Then
                playerInventory(blockType) = If(blockType = BlockType.Dirt, 32, 0)
            End If
        Next
        playerInventory(BlockType.Grass) = 16
        playerInventory(BlockType.Stone) = 16
        playerInventory(BlockType.Wood) = 8

        ' Generate block textures
        GenerateBlockTextures()
    End Sub

    Private Sub GenerateBlockTextures()
        For Each blockType In [Enum].GetValues(GetType(BlockType))
            If blockType <> BlockType.Air Then
                Dim baseColor = blockColors(blockType)
                Using bmp As New Bitmap(16, 16)
                    Using g As Graphics = Graphics.FromImage(bmp)
                        ' Create textured appearance
                        For x = 0 To 15
                            For y = 0 To 15
                                Dim variation = random.Next(-10, 11)
                                Dim r = Math.Max(0, Math.Min(255, baseColor.R + variation))
                                Dim gColor = Math.Max(0, Math.Min(255, baseColor.G + variation))
                                Dim b = Math.Max(0, Math.Min(255, baseColor.B + variation))
                                Using p As New Pen(Color.FromArgb(r, gColor, b))
                                    g.DrawRectangle(p, x, y, 1, 1)
                                End Using
                            Next
                        Next
                    End Using
                    blockTextures(blockType) = New TextureBrush(bmp)
                End Using
            End If
        Next
    End Sub

    Private Sub InitializeWorld()
        ReDim world(WORLD_SIZE, 128, WORLD_SIZE)

        ' Generate terrain using Perlin noise
        For x = 0 To WORLD_SIZE - 1
            For z = 0 To WORLD_SIZE - 1
                ' Base terrain height
                Dim height = CInt(noise.OctavePerlin(x * 0.01, z * 0.01, 0, 4, 0.5) * 30 + 40)
                Dim biome = noise.OctavePerlin(x * 0.005, z * 0.005, 0, 2, 0.5)

                For y = 0 To 127
                    If y = 0 Then
                        world(x, y, z) = BlockType.Bedrock
                    ElseIf y < height - 3 Then
                        ' Stone layer with ores
                        If random.Next(100) < 5 Then
                            world(x, y, z) = If(random.NextDouble() < 0.3, BlockType.CoalOre, BlockType.IronOre)
                        Else
                            world(x, y, z) = BlockType.Stone
                        End If
                    ElseIf y < height - 1 Then
                        world(x, y, z) = BlockType.Dirt
                    ElseIf y < height Then
                        If biome > 0.6 Then
                            world(x, y, z) = BlockType.Sand
                        Else
                            world(x, y, z) = BlockType.Grass
                        End If
                    ElseIf y <= WATER_LEVEL Then
                        world(x, y, z) = BlockType.Water
                    Else
                        world(x, y, z) = BlockType.Air
                    End If
                Next

                ' Generate trees
                If world(x, height, z) = BlockType.Grass AndAlso random.Next(100) < 3 Then
                    GenerateTree(x, height + 1, z)
                End If
            Next
        Next
    End Sub

    Private Sub GenerateTree(x As Integer, y As Integer, z As Integer)
        Dim treeHeight = random.Next(4, 7)

        ' Generate trunk
        For i = 0 To treeHeight - 1
            If y + i < 127 Then
                SetBlock(x, y + i, z, BlockType.Wood)
            End If
        Next

        ' Generate leaves
        For dx = -2 To 2
            For dz = -2 To 2
                For dy = 0 To 2
                    If x + dx >= 0 AndAlso x + dx < WORLD_SIZE AndAlso
                       z + dz >= 0 AndAlso z + dz < WORLD_SIZE AndAlso
                       y + treeHeight + dy < 127 Then

                        If Math.Abs(dx) + Math.Abs(dz) + Math.Abs(dy) < 4 Then
                            SetBlock(x + dx, y + treeHeight + dy, z + dz, BlockType.Leaves)
                        End If
                    End If
                Next
            Next
        Next
    End Sub

    Private Sub GameLoop(sender As Object, e As EventArgs)
        HandleInput()
        UpdatePhysics()
        RenderWorld()
        UpdateFPS()
        Me.Invalidate()
    End Sub

    Private Sub HandleInput()
        Dim moveSpeed As Single = 0.2F
        Dim mouseSensitivity As Single = 0.002F

        ' Rotation from mouse
        playerRotation.Y += (mousePosition.X - Me.ClientSize.Width / 2) * mouseSensitivity
        playerRotation.X -= (mousePosition.Y - Me.ClientSize.Height / 2) * mouseSensitivity
        playerRotation.X = Math.Max(-Math.PI / 2, Math.Min(Math.PI / 2, playerRotation.X))

        ' Reset mouse to center
        If Me.Focused Then
            Cursor.Position = Me.PointToScreen(New Point(Me.ClientSize.Width \ 2, Me.ClientSize.Height \ 2))
        End If

        ' Movement
        Dim moveVector As New Vector3(0, 0, 0)

        If keyState.Contains(Keys.W) Then moveVector.Z += moveSpeed
        If keyState.Contains(Keys.S) Then moveVector.Z -= moveSpeed
        If keyState.Contains(Keys.A) Then moveVector.X -= moveSpeed
        If keyState.Contains(Keys.D) Then moveVector.X += moveSpeed
        If keyState.Contains(Keys.Space) AndAlso playerOnGround Then playerVelocity.Y = 0.4F
        If keyState.Contains(Keys.ShiftKey) Then moveSpeed *= 1.5F

        ' Apply rotation to movement
        Dim rotatedX = moveVector.X * Math.Cos(playerRotation.Y) - moveVector.Z * Math.Sin(playerRotation.Y)
        Dim rotatedZ = moveVector.X * Math.Sin(playerRotation.Y) + moveVector.Z * Math.Cos(playerRotation.Y)

        playerVelocity.X = CSng(rotatedX)
        playerVelocity.Z = CSng(rotatedZ)
    End Sub

    Private Sub UpdatePhysics()
        ' Apply gravity
        If Not playerOnGround Then
            playerVelocity.Y -= 0.02F
        End If

        ' Check collisions
        playerOnGround = False
        Dim newPosition = playerPosition + playerVelocity

        ' Simple AABB collision detection
        For dx = -1 To 1
            For dy = -1 To 2
                For dz = -1 To 1
                    Dim checkX = CInt(Math.Floor(newPosition.X + dx * 0.3))
                    Dim checkY = CInt(Math.Floor(newPosition.Y + dy * 1.8))
                    Dim checkZ = CInt(Math.Floor(newPosition.Z + dz * 0.3))

                    If IsSolidBlock(checkX, checkY, checkZ) Then
                        ' Collision response
                        If dy = -1 Then playerOnGround = True
                        playerVelocity.Y = 0
                        newPosition.Y = playerPosition.Y
                    End If
                Next
            Next
        Next

        playerPosition = newPosition

        ' Apply friction
        playerVelocity.X *= 0.8F
        playerVelocity.Z *= 0.8F

        ' Keep player in world bounds
        playerPosition.X = Math.Max(1, Math.Min(WORLD_SIZE - 2, playerPosition.X))
        playerPosition.Z = Math.Max(1, Math.Min(WORLD_SIZE - 2, playerPosition.Z))
        playerPosition.Y = Math.Max(1, Math.Min(126, playerPosition.Y))
    End Sub

    Private Sub RenderWorld()
        graphicsBuffer.Clear(Color.SkyBlue)

        ' Setup camera
        camera.Position = playerPosition
        camera.Rotation = playerRotation

        ' Render chunks
        For x = -RENDER_DISTANCE To RENDER_DISTANCE
            For z = -RENDER_DISTANCE To RENDER_DISTANCE
                Dim chunkX = CInt(Math.Floor(playerPosition.X / CHUNK_SIZE)) + x
                Dim chunkZ = CInt(Math.Floor(playerPosition.Z / CHUNK_SIZE)) + z

                If chunkX >= 0 AndAlso chunkX < WORLD_SIZE / CHUNK_SIZE AndAlso
                   chunkZ >= 0 AndAlso chunkZ < WORLD_SIZE / CHUNK_SIZE Then
                    RenderChunk(chunkX * CHUNK_SIZE, chunkZ * CHUNK_SIZE)
                End If
            Next
        Next

        ' Render UI
        RenderUI()
    End Sub

    Private Sub RenderChunk(startX As Integer, startZ As Integer)
        For x = startX To Math.Min(startX + CHUNK_SIZE - 1, WORLD_SIZE - 1)
            For z = startZ To Math.Min(startZ + CHUNK_SIZE - 1, WORLD_SIZE - 1)
                For y = 0 To 127
                    Dim blockType = world(x, y, z)
                    If blockType <> BlockType.Air Then
                        RenderBlock(x, y, z, blockType)
                    End If
                Next
            Next
        Next
    End Sub

    Private Sub RenderBlock(x As Integer, y As Integer, z As Integer, blockType As BlockType)
        ' Simple 3D projection (isometric-like view)
        Dim screenX = CSng((x - playerPosition.X) * Math.Cos(playerRotation.Y) - 
                          (z - playerPosition.Z) * Math.Sin(playerRotation.Y)) * BLOCK_SIZE + Me.ClientSize.Width / 2
        Dim screenY = CSng((y - playerPosition.Y) + 
                          ((x - playerPosition.X) * Math.Sin(playerRotation.Y) + 
                           (z - playerPosition.Z) * Math.Cos(playerRotation.Y)) * 0.5) * BLOCK_SIZE + Me.ClientSize.Height / 2

        Dim size = BLOCK_SIZE
        If blockType = BlockType.Water Then size = CInt(BLOCK_SIZE * 0.9)

        Dim rect As New Rectangle(CInt(screenX - size / 2), CInt(screenY - size / 2), size, size)

        ' Draw block
        If blockTextures.ContainsKey(blockType) Then
            graphicsBuffer.FillRectangle(blockTextures(blockType), rect)
        Else
            Using brush As New SolidBrush(blockColors(blockType))
                graphicsBuffer.FillRectangle(brush, rect)
            End Using
        End If

        ' Draw block outline
        graphicsBuffer.DrawRectangle(Pens.Black, rect)
    End Sub

    Private Sub RenderUI()
        ' Draw crosshair
        Dim centerX = Me.ClientSize.Width / 2
        Dim centerY = Me.ClientSize.Height / 2
        graphicsBuffer.DrawLine(Pens.White, centerX - 10, centerY, centerX + 10, centerY)
        graphicsBuffer.DrawLine(Pens.White, centerX, centerY - 10, centerX, centerY + 10)

        ' Draw hotbar
        Dim hotbarWidth = hotbarSlots * 50
        Dim hotbarX = (Me.ClientSize.Width - hotbarWidth) / 2
        Dim hotbarY = Me.ClientSize.Height - 60

        For i = 0 To hotbarSlots - 1
            Dim rect As New Rectangle(CInt(hotbarX + i * 50), CInt(hotbarY), 40, 40)
            graphicsBuffer.FillRectangle(Brushes.DarkGray, rect)
            graphicsBuffer.DrawRectangle(Pens.White, rect)

            If i = hotbarSelection Then
                graphicsBuffer.DrawRectangle(New Pen(Color.Yellow, 3), rect)
            End If
        Next

        ' Draw FPS
        graphicsBuffer.DrawString($"FPS: {lastFps}", New Font("Arial", 12), Brushes.White, 10, 10)
        graphicsBuffer.DrawString($"Position: {playerPosition.X:F1}, {playerPosition.Y:F1}, {playerPosition.Z:F1}", 
                                New Font("Arial", 12), Brushes.White, 10, 30)
        graphicsBuffer.DrawString($"Health: {playerHealth} ❤ Hunger: {playerHunger}", 
                                New Font("Arial", 12), Brushes.White, 10, 50)
    End Sub

    Private Sub UpdateFPS()
        fpsCounter += 1
        If fpsTimer.ElapsedMilliseconds >= 1000 Then
            lastFps = fpsCounter
            fpsCounter = 0
            fpsTimer.Restart()
        End If
    End Sub

    ' Input Handlers
    Private Sub OnKeyDown(sender As Object, e As KeyEventArgs)
        keyState.Add(e.KeyCode)

        ' Hotbar selection
        If e.KeyCode >= Keys.D1 AndAlso e.KeyCode <= Keys.D9 Then
            hotbarSelection = e.KeyCode - Keys.D1
        End If

        ' Block selection
        Select Case e.KeyCode
            Case Keys.F1 : selectedBlock = BlockType.Grass
            Case Keys.F2 : selectedBlock = BlockType.Dirt
            Case Keys.F3 : selectedBlock = BlockType.Stone
            Case Keys.F4 : selectedBlock = BlockType.Wood
            Case Keys.Escape : Me.Close()
        End Select
    End Sub

    Private Sub OnKeyUp(sender As Object, e As KeyEventArgs)
        keyState.Remove(e.KeyCode)
    End Sub

    Private Sub OnMouseDown(sender As Object, e As MouseEventArgs)
        mouseDown = True
        If e.Button = MouseButtons.Left Then
            BreakBlock()
        ElseIf e.Button = MouseButtons.Right Then
            PlaceBlock()
        End If
    End Sub

    Private Sub OnMouseUp(sender As Object, e As MouseEventArgs)
        mouseDown = False
    End Sub

    Private Sub OnMouseMove(sender As Object, e As MouseEventArgs)
        mousePosition = e.Location
    End Sub

    Private Sub OnMouseWheel(sender As Object, e As MouseEventArgs)
        hotbarSelection = (hotbarSelection + Math.Sign(e.Delta)) Mod hotbarSlots
        If hotbarSelection < 0 Then hotbarSelection += hotbarSlots
    End Sub

    Private Sub BreakBlock()
        Dim target = GetTargetBlock()
        If target.HasValue Then
            Dim blockType = GetBlock(target.Value.X, target.Value.Y, target.Value.Z)
            If blockType <> BlockType.Air AndAlso blockType <> BlockType.Bedrock Then
                SetBlock(target.Value.X, target.Value.Y, target.Value.Z, BlockType.Air)
                ' Add to inventory
                playerInventory(blockType) = playerInventory(blockType) + 1
            End If
        End If
    End Sub

    Private Sub PlaceBlock()
        If playerInventory(selectedBlock) > 0 Then
            Dim target = GetTargetBlock()
            If target.HasValue Then
                Dim placePos = GetAdjacentPosition(target.Value)
                If placePos.HasValue AndAlso GetBlock(placePos.Value.X, placePos.Value.Y, placePos.Value.Z) = BlockType.Air Then
                    SetBlock(placePos.Value.X, placePos.Value.Y, placePos.Value.Z, selectedBlock)
                    playerInventory(selectedBlock) = playerInventory(selectedBlock) - 1
                End If
            End If
        End If
    End Sub

    Private Function GetTargetBlock() As Vector3?
        ' Simple raycasting for block selection
        For distance = 1 To 5 Step 0.5
            Dim checkPos = playerPosition + camera.Forward * distance
            Dim blockPos As New Vector3(Math.Floor(checkPos.X), Math.Floor(checkPos.Y), Math.Floor(checkPos.Z))

            If IsSolidBlock(CInt(blockPos.X), CInt(blockPos.Y), CInt(blockPos.Z)) Then
                Return blockPos
            End If
        Next
        Return Nothing
    End Function

    Private Function GetAdjacentPosition(pos As Vector3) As Vector3?
        Dim directions() As Vector3 = {
            New Vector3(1, 0, 0), New Vector3(-1, 0, 0),
            New Vector3(0, 1, 0), New Vector3(0, -1, 0),
            New Vector3(0, 0, 1), New Vector3(0, 0, -1)
        }

        For Each dir In directions
            Dim adjacent = pos + dir
            If Not IsSolidBlock(CInt(adjacent.X), CInt(adjacent.Y), CInt(adjacent.Z)) Then
                Return adjacent
            End If
        Next
        Return Nothing
    End Function

    ' Block management
    Private Function GetBlock(x As Integer, y As Integer, z As Integer) As BlockType
        If x >= 0 AndAlso x < WORLD_SIZE AndAlso y >= 0 AndAlso y < 128 AndAlso z >= 0 AndAlso z < WORLD_SIZE Then
            Return world(x, y, z)
        End If
        Return BlockType.Air
    End Function

    Private Sub SetBlock(x As Integer, y As Integer, z As Integer, blockType As BlockType)
        If x >= 0 AndAlso x < WORLD_SIZE AndAlso y >= 0 AndAlso y < 128 AndAlso z >= 0 AndAlso z < WORLD_SIZE Then
            world(x, y, z) = blockType
        End If
    End Sub

    Private Function IsSolidBlock(x As Integer, y As Integer, z As Integer) As Boolean
        Return solidBlocks.Contains(GetBlock(x, y, z))
    End Function

    Protected Overrides Sub OnPaint(e As PaintEventArgs)
        e.Graphics.DrawImage(renderBuffer, 0, 0)
    End Sub

    ' Support Classes
    Private Structure Vector3
        Public X, Y, Z As Single
        Public Sub New(x As Single, y As Single, z As Single)
            Me.X = x
            Me.Y = y
            Me.Z = z
        End Sub
        Public Shared Operator +(a As Vector3, b As Vector3) As Vector3
            Return New Vector3(a.X + b.X, a.Y + b.Y, a.Z + b.Z)
        End Operator
        Public Shared Operator *(v As Vector3, scalar As Single) As Vector3
            Return New Vector3(v.X * scalar, v.Y * scalar, v.Z * scalar)
        End Operator
    End Structure

    Private Structure Vector2
        Public X, Y As Single
        Public Sub New(x As Single, y As Single)
            Me.X = x
            Me.Y = y
        End Sub
    End Structure

    Private Class Camera
        Public Position As Vector3
        Public Rotation As Vector2
        Public ReadOnly Property Forward As Vector3
            Get
                Return New Vector3(
                    CSng(Math.Sin(Rotation.Y) * Math.Cos(Rotation.X)),
                    CSng(Math.Sin(Rotation.X)),
                    CSng(Math.Cos(Rotation.Y) * Math.Cos(Rotation.X))
                )
            End Get
        End Property
    End Class

    ' Perlin Noise Implementation for Terrain Generation
    Private Class PerlinNoise
        Private ReadOnly perm As Integer()

        Public Sub New()
            perm = Enumerable.Range(0, 256).OrderBy(Function(x) Guid.NewGuid()).ToArray()
            Array.Resize(perm, 512)
            Array.Copy(perm, 0, perm, 256, 256)
        End Sub

        Public Function OctavePerlin(x As Double, y As Double, z As Double, octaves As Integer, persistence As Double) As Double
            Dim total As Double = 0
            Dim frequency As Double = 1
            Dim amplitude As Double = 1
            Dim maxValue As Double = 0

            For i = 0 To octaves - 1
                total += Perlin(x * frequency, y * frequency, z * frequency) * amplitude
                maxValue += amplitude
                amplitude *= persistence
                frequency *= 2
            Next

            Return total / maxValue
        End Function

        Private Function Perlin(x As Double, y As Double, z As Double) As Double
            Dim xi = CInt(Math.Floor(x)) And 255
            Dim yi = CInt(Math.Floor(y)) And 255
            Dim zi = CInt(Math.Floor(z)) And 255

            Dim xf = x - Math.Floor(x)
            Dim yf = y - Math.Floor(y)
            Dim zf = z - Math.Floor(z)

            Dim u = Fade(xf)
            Dim v = Fade(yf)
            Dim w = Fade(zf)

            Dim a = perm(xi) + yi
            Dim aa = perm(a) + zi
            Dim ab = perm(a + 1) + zi
            Dim b = perm(xi + 1) + yi
            Dim ba = perm(b) + zi
            Dim bb = perm(b + 1) + zi

            Dim x1, x2, y1, y2 As Double
            x1 = Lerp(Grad(perm(aa), xf, yf, zf), Grad(perm(ba), xf - 1, yf, zf), u)
            x2 = Lerp(Grad(perm(ab), xf, yf - 1, zf), Grad(perm(bb), xf - 1, yf - 1, zf), u)
            y1 = Lerp(x1, x2, v)
            x1 = Lerp(Grad(perm(aa + 1), xf, yf, zf - 1), Grad(perm(ba + 1), xf - 1, yf, zf - 1), u)
            x2 = Lerp(Grad(perm(ab + 1), xf, yf - 1, zf - 1), Grad(perm(bb + 1), xf - 1, yf - 1, zf - 1), u)
            y2 = Lerp(x1, x2, v)

            Return (Lerp(y1, y2, w) + 1) / 2
        End Function

        Private Function Fade(t As Double) As Double
            Return t * t * t * (t * (t * 6 - 15) + 10)
        End Function

        Private Function Lerp(a As Double, b As Double, t As Double) As Double
            Return a + t * (b - a)
        End Function

        Private Function Grad(hash As Integer, x As Double, y As Double, z As Double) As Double
            Dim h = hash And 15
            Dim u = If(h < 8, x, y)
            Dim v = If(h < 4, y, If(h = 12 OrElse h = 14, x, z))
            Return If((h And 1) = 0, u, -u) + If((h And 2) = 0, v, -v)
        End Function
    End Class
End Class

' Application Entry Point
Module Program
    <STAThread>
    Sub Main()
        Application.EnableVisualStyles()
        Application.SetCompatibleTextRenderingDefault(False)
        Application.Run(New MinecraftClone())
    End Sub
End Module