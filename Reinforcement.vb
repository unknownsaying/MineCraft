Imports System
Imports System.Collections.Generic
Imports System.Linq
Imports System.Text
Imports System.Drawing
Imports System.Windows.Forms
Imports System.Diagnostics

Public Class ReinforcementLearningDemo
    Inherits Form

    ' RL Algorithms
    Private Enum RLAlgorithm
        QLearning
        SARSA
        DeepQLearning
        PolicyGradient
    End Enum

    ' Environments
    Private Enum EnvironmentType
        GridWorld
        CartPole
        MountainCar
        FrozenLake
    End Enum

    ' Q-Learning Parameters
    Private learningRate As Double = 0.1
    Private discountFactor As Double = 0.9
    Private explorationRate As Double = 0.3
    Private explorationDecay As Double = 0.995
    Private minExplorationRate As Double = 0.01

    ' Neural Network Parameters
    Private hiddenLayerSize As Integer = 64
    Private batchSize As Integer = 32
    Private replayMemorySize As Integer = 1000
    Private targetUpdateFrequency As Integer = 100

    ' Training
    Private currentAlgorithm As RLAlgorithm = RLAlgorithm.QLearning
    Private currentEnvironment As EnvironmentType = EnvironmentType.GridWorld
    Private isTraining As Boolean = False
    Private trainingEpisode As Integer = 0
    Private totalSteps As Integer = 0
    Private totalReward As Double = 0

    ' Q-Tables and Neural Networks
    Private qTable As Dictionary(Of String, Double())
    Private neuralNetwork As DeepQNetwork
    Private replayMemory As New List(Of Experience)
    Private targetNetwork As DeepQNetwork

    ' Environments
    Private gridWorld As GridWorldEnvironment
    Private cartPole As CartPoleEnvironment
    Private mountainCar As MountainCarEnvironment
    Private frozenLake As FrozenLakeEnvironment

    ' UI Components
    Private WithEvents btnStartTraining As New Button()
    Private WithEvents btnReset As New Button()
    Private WithEvents cmbAlgorithm As New ComboBox()
    Private WithEvents cmbEnvironment As New ComboBox()
    Private WithEvents picVisualization As New PictureBox()
    Private WithEvents lblStats As New Label()
    Private WithEvents timer As New Timer()

    ' Rendering
    Private renderBuffer As Bitmap
    Private graphicsBuffer As Graphics

    Public Sub New()
        InitializeComponent()
        InitializeRL()
        InitializeEnvironments()
    End Sub

    Private Sub InitializeComponent()
        Me.Text = "Reinforcement Learning Demo - VB.NET"
        Me.Size = New Size(1000, 700)
        Me.StartPosition = FormStartPosition.CenterScreen
        Me.DoubleBuffered = True

        ' Setup controls
        SetupControls()

        ' Initialize rendering
        renderBuffer = New Bitmap(picVisualization.Width, picVisualization.Height)
        graphicsBuffer = Graphics.FromImage(renderBuffer)

        ' Setup timer
        timer.Interval = 50
        AddHandler timer.Tick, AddressOf GameLoop
    End Sub

    Private Sub SetupControls()
        ' Algorithm selection
        Dim lblAlgorithm As New Label() With {.Text = "Algorithm:", .Location = New Point(10, 10), .Size = New Size(80, 20)}
        cmbAlgorithm.Location = New Point(90, 10)
        cmbAlgorithm.Size = New Size(120, 20)
        cmbAlgorithm.Items.AddRange({"Q-Learning", "SARSA", "Deep Q-Learning", "Policy Gradient"})
        cmbAlgorithm.SelectedIndex = 0

        ' Environment selection
        Dim lblEnvironment As New Label() With {.Text = "Environment:", .Location = New Point(220, 10), .Size = New Size(80, 20)}
        cmbEnvironment.Location = New Point(300, 10)
        cmbEnvironment.Size = New Size(120, 20)
        cmbEnvironment.Items.AddRange({"Grid World", "Cart Pole", "Mountain Car", "Frozen Lake"})
        cmbEnvironment.SelectedIndex = 0

        ' Buttons
        btnStartTraining.Text = "Start Training"
        btnStartTraining.Location = New Point(430, 10)
        btnStartTraining.Size = New Size(100, 25)

        btnReset.Text = "Reset"
        btnReset.Location = New Point(540, 10)
        btnReset.Size = New Size(80, 25)

        ' Stats label
        lblStats.Location = New Point(630, 10)
        lblStats.Size = New Size(350, 50)
        lblStats.Text = "Episode: 0 | Steps: 0 | Reward: 0.00"

        ' Visualization
        picVisualization.Location = New Point(10, 50)
        picVisualization.Size = New Size(800, 600)
        picVisualization.BackColor = Color.White
        picVisualization.BorderStyle = BorderStyle.FixedSingle

        ' Add controls to form
        Me.Controls.AddRange({lblAlgorithm, cmbAlgorithm, lblEnvironment, cmbEnvironment,
                            btnStartTraining, btnReset, lblStats, picVisualization})

        ' Event handlers
        AddHandler cmbAlgorithm.SelectedIndexChanged, AddressOn AlgorithmChanged
        AddHandler cmbEnvironment.SelectedIndexChanged, AddressOn EnvironmentChanged
        AddHandler btnStartTraining.Click, AddressOn ToggleTraining
        AddHandler btnReset.Click, AddressOn ResetTraining
    End Sub

    Private Sub InitializeRL()
        qTable = New Dictionary(Of String, Double())()
        neuralNetwork = New DeepQNetwork(4, hiddenLayerSize, 2) ' Default for CartPole
        targetNetwork = New DeepQNetwork(4, hiddenLayerSize, 2)
        targetNetwork.CopyWeights(neuralNetwork)
    End Sub

    Private Sub InitializeEnvironments()
        gridWorld = New GridWorldEnvironment(8, 8)
        cartPole = New CartPoleEnvironment()
        mountainCar = New MountainCarEnvironment()
        frozenLake = New FrozenLakeEnvironment(8, 8)
    End Sub

    Private Sub GameLoop(sender As Object, e As EventArgs)
        If isTraining Then
            TrainStep()
        End If
        Render()
        UpdateStats()
    End Sub

    Private Sub TrainStep()
        trainingEpisode += 1
        Dim episodeReward As Double = 0
        Dim steps As Integer = 0

        ResetEnvironment()

        While Not IsTerminal() AndAlso steps < 1000
            Dim state = GetCurrentState()
            Dim action As Integer = SelectAction(state)
            Dim reward As Double = TakeAction(action)
            Dim nextState = GetCurrentState()
            Dim isTerminal = IsTerminal()

            ' Store experience
            StoreExperience(state, action, reward, nextState, isTerminal)

            ' Learn from experience
            LearnFromExperience()

            episodeReward += reward
            steps += 1
            totalSteps += 1

            If isTerminal Then
                Exit While
            End If
        End While

        totalReward = episodeReward
        explorationRate = Math.Max(minExplorationRate, explorationRate * explorationDecay)
    End Sub

    Private Function SelectAction(state As Double()) As Integer
        Select Case currentAlgorithm
            Case RLAlgorithm.QLearning, RLAlgorithm.SARSA
                Return SelectActionTabular(state)
            Case RLAlgorithm.DeepQLearning
                Return SelectActionDeepQ(state)
            Case RLAlgorithm.PolicyGradient
                Return SelectActionPolicyGradient(state)
            Case Else
                Return 0
        End Select
    End Function

    Private Function SelectActionTabular(state As Double()) As Integer
        Dim stateKey = StateToString(state)

        ' Initialize Q-values if new state
        If Not qTable.ContainsKey(stateKey) Then
            qTable(stateKey) = New Double(GetActionCount() - 1) {}
        End If

        ' Epsilon-greedy exploration
        If RandomGenerator.NextDouble() < explorationRate Then
            Return RandomGenerator.Next(0, GetActionCount())
        Else
            Dim qValues = qTable(stateKey)
            Return Array.IndexOf(qValues, qValues.Max())
        End If
    End Function

    Private Function SelectActionDeepQ(state As Double()) As Integer
        If RandomGenerator.NextDouble() < explorationRate Then
            Return RandomGenerator.Next(0, GetActionCount())
        Else
            Dim qValues = neuralNetwork.Predict(state)
            Return Array.IndexOf(qValues, qValues.Max())
        End If
    End Function

    Private Function SelectActionPolicyGradient(state As Double()) As Integer
        Dim probabilities = neuralNetwork.PredictProbabilities(state)
        Dim randomValue = RandomGenerator.NextDouble()
        Dim cumulative As Double = 0

        For i As Integer = 0 To probabilities.Length - 1
            cumulative += probabilities(i)
            If randomValue <= cumulative Then
                Return i
            End If
        Next

        Return probabilities.Length - 1
    End Function

    Private Sub LearnFromExperience()
        Select Case currentAlgorithm
            Case RLAlgorithm.QLearning
                LearnQLearning()
            Case RLAlgorithm.SARSA
                LearnSARSA()
            Case RLAlgorithm.DeepQLearning
                LearnDeepQLearning()
            Case RLAlgorithm.PolicyGradient
                LearnPolicyGradient()
        End Select
    End Sub

    Private Sub LearnQLearning()
        If replayMemory.Count < batchSize Then Return

        Dim batch = GetRandomBatch()
        For Each experience In batch
            Dim stateKey = StateToString(experience.State)
            Dim nextStateKey = StateToString(experience.NextState)

            ' Initialize Q-values if needed
            If Not qTable.ContainsKey(stateKey) Then
                qTable(stateKey) = New Double(GetActionCount() - 1) {}
            End If
            If Not qTable.ContainsKey(nextStateKey) Then
                qTable(nextStateKey) = New Double(GetActionCount() - 1) {}
            End If

            Dim currentQ = qTable(stateKey)(experience.Action)
            Dim maxNextQ = qTable(nextStateKey).Max()

            Dim targetQ = experience.Reward
            If Not experience.IsTerminal Then
                targetQ += discountFactor * maxNextQ
            End If

            qTable(stateKey)(experience.Action) += learningRate * (targetQ - currentQ)
        Next
    End Sub

    Private Sub LearnSARSA()
        If replayMemory.Count < 2 Then Return

        Dim lastExperience = replayMemory(replayMemory.Count - 2)
        Dim currentExperience = replayMemory(replayMemory.Count - 1)

        Dim stateKey = StateToString(lastExperience.State)
        Dim nextStateKey = StateToString(currentExperience.State)

        If Not qTable.ContainsKey(stateKey) Then
            qTable(stateKey) = New Double(GetActionCount() - 1) {}
        End If
        If Not qTable.ContainsKey(nextStateKey) Then
            qTable(nextStateKey) = New Double(GetActionCount() - 1) {}
        End If

        Dim currentQ = qTable(stateKey)(lastExperience.Action)
        Dim nextQ = qTable(nextStateKey)(currentExperience.Action)

        Dim targetQ = lastExperience.Reward
        If Not lastExperience.IsTerminal Then
            targetQ += discountFactor * nextQ
        End If

        qTable(stateKey)(lastExperience.Action) += learningRate * (targetQ - currentQ)
    End Sub

    Private Sub LearnDeepQLearning()
        If replayMemory.Count < batchSize Then Return

        Dim batch = GetRandomBatch()
        Dim states(batchSize - 1)() As Double
        Dim targets(batchSize - 1)() As Double

        For i As Integer = 0 To batchSize - 1
            Dim experience = batch(i)
            states(i) = experience.State

            Dim currentQ = neuralNetwork.Predict(experience.State)
            Dim nextQ = targetNetwork.Predict(experience.NextState)
            Dim maxNextQ = nextQ.Max()

            Dim targetQ = experience.Reward
            If Not experience.IsTerminal Then
                targetQ += discountFactor * maxNextQ
            End If

            currentQ(experience.Action) = targetQ
            targets(i) = currentQ
        Next

        neuralNetwork.Train(states, targets, batchSize, 1)

        ' Update target network periodically
        If totalSteps Mod targetUpdateFrequency = 0 Then
            targetNetwork.CopyWeights(neuralNetwork)
        End If
    End Sub

    Private Sub LearnPolicyGradient()
        If replayMemory.Count < batchSize Then Return

        Dim batch = GetRandomBatch()
        Dim states(batchSize - 1)() As Double
        Dim actions(batchSize - 1) As Integer
        Dim rewards(batchSize - 1) As Double

        For i As Integer = 0 To batchSize - 1
            Dim experience = batch(i)
            states(i) = experience.State
            actions(i) = experience.Action
            rewards(i) = experience.Reward
        Next

        ' Normalize rewards
        Dim meanReward = rewards.Average()
        Dim stdReward = Math.Sqrt(rewards.Select(Function(r) (r - meanReward) ^ 2).Average())
        If stdReward = 0 Then stdReward = 1

        For i As Integer = 0 To batchSize - 1
            rewards(i) = (rewards(i) - meanReward) / stdReward
        Next

        neuralNetwork.TrainPolicyGradient(states, actions, rewards, batchSize)
    End Sub

    Private Sub StoreExperience(state As Double(), action As Integer, reward As Double, nextState As Double(), isTerminal As Boolean)
        Dim experience As New Experience With {
            .State = state,
            .Action = action,
            .Reward = reward,
            .NextState = nextState,
            .IsTerminal = isTerminal
        }

        replayMemory.Add(experience)

        ' Maintain replay memory size
        If replayMemory.Count > replayMemorySize Then
            replayMemory.RemoveAt(0)
        End If
    End Sub

    Private Function GetRandomBatch() As List(Of Experience)
        Return replayMemory.OrderBy(Function(x) RandomGenerator.Next()).Take(batchSize).ToList()
    End Function

    ' Environment Methods
    Private Function GetCurrentState() As Double()
        Select Case currentEnvironment
            Case EnvironmentType.GridWorld
                Return gridWorld.GetState()
            Case EnvironmentType.CartPole
                Return cartPole.GetState()
            Case EnvironmentType.MountainCar
                Return mountainCar.GetState()
            Case EnvironmentType.FrozenLake
                Return frozenLake.GetState()
            Case Else
                Return New Double() {0}
        End Select
    End Function

    Private Function TakeAction(action As Integer) As Double
        Select Case currentEnvironment
            Case EnvironmentType.GridWorld
                Return gridWorld.Step(action)
            Case EnvironmentType.CartPole
                Return cartPole.Step(action)
            Case EnvironmentType.MountainCar
                Return mountainCar.Step(action)
            Case EnvironmentType.FrozenLake
                Return frozenLake.Step(action)
            Case Else
                Return 0
        End Select
    End Function

    Private Sub ResetEnvironment()
        Select Case currentEnvironment
            Case EnvironmentType.GridWorld
                gridWorld.Reset()
            Case EnvironmentType.CartPole
                cartPole.Reset()
            Case EnvironmentType.MountainCar
                mountainCar.Reset()
            Case EnvironmentType.FrozenLake
                frozenLake.Reset()
        End Select
    End Function

    Private Function IsTerminal() As Boolean
        Select Case currentEnvironment
            Case EnvironmentType.GridWorld
                Return gridWorld.IsTerminal()
            Case EnvironmentType.CartPole
                Return cartPole.IsTerminal()
            Case EnvironmentType.MountainCar
                Return mountainCar.IsTerminal()
            Case EnvironmentType.FrozenLake
                Return frozenLake.IsTerminal()
            Case Else
                Return False
        End Select
    End Function

    Private Function GetActionCount() As Integer
        Select Case currentEnvironment
            Case EnvironmentType.GridWorld
                Return 4
            Case EnvironmentType.CartPole
                Return 2
            Case EnvironmentType.MountainCar
                Return 3
            Case EnvironmentType.FrozenLake
                Return 4
            Case Else
                Return 1
        End Select
    End Function

    Private Function StateToString(state As Double()) As String
        Return String.Join(",", state.Select(Function(s) Math.Round(s, 2).ToString()))
    End Function

    ' UI Event Handlers
    Private Sub ToggleTraining(sender As Object, e As EventArgs)
        isTraining = Not isTraining
        btnStartTraining.Text = If(isTraining, "Stop Training", "Start Training")
        
        If isTraining Then
            timer.Start()
        Else
            timer.Stop()
        End If
    End Sub

    Private Sub ResetTraining(sender As Object, e As EventArgs)
        trainingEpisode = 0
        totalSteps = 0
        totalReward = 0
        explorationRate = 0.3
        qTable.Clear()
        replayMemory.Clear()
        ResetEnvironment()
        InitializeRL()
    End Sub

    Private Sub AlgorithmChanged(sender As Object, e As EventArgs)
        currentAlgorithm = CType(cmbAlgorithm.SelectedIndex, RLAlgorithm)
        ResetTraining(Nothing, Nothing)
    End Sub

    Private Sub EnvironmentChanged(sender As Object, e As EventArgs)
        currentEnvironment = CType(cmbEnvironment.SelectedIndex, EnvironmentType)
        
        ' Update neural network architecture based on environment
        Dim stateSize As Integer = GetStateSize()
        Dim actionSize As Integer = GetActionCount()
        
        neuralNetwork = New DeepQNetwork(stateSize, hiddenLayerSize, actionSize)
        targetNetwork = New DeepQNetwork(stateSize, hiddenLayerSize, actionSize)
        targetNetwork.CopyWeights(neuralNetwork)
        
        ResetTraining(Nothing, Nothing)
    End Sub

    Private Function GetStateSize() As Integer
        Select Case currentEnvironment
            Case EnvironmentType.GridWorld : Return 2
            Case EnvironmentType.CartPole : Return 4
            Case EnvironmentType.MountainCar : Return 2
            Case EnvironmentType.FrozenLake : Return 2
            Case Else : Return 1
        End Select
    End Function

    Private Sub UpdateStats()
        lblStats.Text = $"Episode: {trainingEpisode} | Steps: {totalSteps} | Reward: {totalReward:F2} | " &
                       $"Exploration: {explorationRate:F3} | Memory: {replayMemory.Count}"
    End Sub

    Private Sub Render()
        graphicsBuffer.Clear(Color.White)

        Select Case currentEnvironment
            Case EnvironmentType.GridWorld
                gridWorld.Render(graphicsBuffer, picVisualization.Width, picVisualization.Height)
            Case EnvironmentType.CartPole
                cartPole.Render(graphicsBuffer, picVisualization.Width, picVisualization.Height)
            Case EnvironmentType.MountainCar
                mountainCar.Render(graphicsBuffer, picVisualization.Width, picVisualization.Height)
            Case EnvironmentType.FrozenLake
                frozenLake.Render(graphicsBuffer, picVisualization.Width, picVisualization.Height)
        End Select

        ' Draw Q-values or policy if available
        If currentAlgorithm = RLAlgorithm.QLearning OrElse currentAlgorithm = RLAlgorithm.SARSA Then
            DrawQValues(graphicsBuffer)
        End If

        picVisualization.Image = renderBuffer
    End Sub

    Private Sub DrawQValues(g As Graphics)
        If qTable.Count = 0 Then Return

        Dim font As New Font("Arial", 8)
        Dim brush As New SolidBrush(Color.Black)

        For Each kvp In qTable.Take(50) ' Limit drawing to first 50 states
            Dim state = kvp.Key.Split(","c).Select(Function(s) Double.Parse(s)).ToArray()
            Dim qValues = kvp.Value

            ' Convert state to screen coordinates (simplified)
            Dim x As Integer = CInt(state(0) * 50 + 100)
            Dim y As Integer = CInt(state(1) * 50 + 100)

            For a As Integer = 0 To qValues.Length - 1
                Dim qText = $"A{a}: {qValues(a):F2}"
                g.DrawString(qText, font, brush, x, y + a * 12)
            Next
        Next
    End Sub

    ' Support Classes
    Private Class Experience
        Public State As Double()
        Public Action As Integer
        Public Reward As Double
        Public NextState As Double()
        Public IsTerminal As Boolean
    End Class

    Private Class DeepQNetwork
        Private inputSize As Integer
        Private hiddenSize As Integer
        Private outputSize As Integer
        Private weights1(,) As Double
        Private weights2(,) As Double
        Private biases1() As Double
        Private biases2() As Double
        Private learningRate As Double = 0.001

        Public Sub New(inputSize As Integer, hiddenSize As Integer, outputSize As Integer)
            Me.inputSize = inputSize
            Me.hiddenSize = hiddenSize
            Me.outputSize = outputSize

            InitializeWeights()
        End Sub

        Private Sub InitializeWeights()
            weights1 = New Double(inputSize - 1, hiddenSize - 1) {}
            weights2 = New Double(hiddenSize - 1, outputSize - 1) {}
            biases1 = New Double(hiddenSize - 1) {}
            biases2 = New Double(outputSize - 1) {}

            Dim random As New Random()

            For i As Integer = 0 To inputSize - 1
                For j As Integer = 0 To hiddenSize - 1
                    weights1(i, j) = random.NextDouble() * 2 - 1
                Next
            Next

            For i As Integer = 0 To hiddenSize - 1
                For j As Integer = 0 To outputSize - 1
                    weights2(i, j) = random.NextDouble() * 2 - 1
                Next
                biases1(i) = random.NextDouble() * 2 - 1
            Next

            For i As Integer = 0 To outputSize - 1
                biases2(i) = random.NextDouble() * 2 - 1
            Next
        End Sub

        Public Function Predict(inputs As Double()) As Double()
            Dim hidden(hiddenSize - 1) As Double

            ' Input to hidden
            For i As Integer = 0 To hiddenSize - 1
                hidden(i) = 0
                For j As Integer = 0 To inputSize - 1
                    hidden(i) += inputs(j) * weights1(j, i)
                Next
                hidden(i) += biases1(i)
                hidden(i) = Math.Tanh(hidden(i)) ' Activation
            Next

            ' Hidden to output
            Dim output(outputSize - 1) As Double
            For i As Integer = 0 To outputSize - 1
                output(i) = 0
                For j As Integer = 0 To hiddenSize - 1
                    output(i) += hidden(j) * weights2(j, i)
                Next
                output(i) += biases2(i)
            Next

            Return output
        End Function

        Public Function PredictProbabilities(inputs As Double()) As Double()
            Dim output = Predict(inputs)
            Dim expOutput = output.Select(Function(x) Math.Exp(x)).ToArray()
            Dim sum = expOutput.Sum()
            Return expOutput.Select(Function(x) x / sum).ToArray()
        End Function

        Public Sub Train(inputs()(), targets()(), batchSize As Integer, epochs As Integer)
            For epoch As Integer = 0 To epochs - 1
                For sample As Integer = 0 To batchSize - 1
                    ' Forward pass
                    Dim hidden(hiddenSize - 1) As Double
                    Dim hiddenInput(hiddenSize - 1) As Double

                    For i As Integer = 0 To hiddenSize - 1
                        hiddenInput(i) = 0
                        For j As Integer = 0 To inputSize - 1
                            hiddenInput(i) += inputs(sample)(j) * weights1(j, i)
                        Next
                        hiddenInput(i) += biases1(i)
                        hidden(i) = Math.Tanh(hiddenInput(i))
                    Next

                    Dim output(outputSize - 1) As Double
                    For i As Integer = 0 To outputSize - 1
                        output(i) = 0
                        For j As Integer = 0 To hiddenSize - 1
                            output(i) += hidden(j) * weights2(j, i)
                        Next
                        output(i) += biases2(i)
                    Next

                    ' Backward pass
                    Dim outputError(outputSize - 1) As Double
                    For i As Integer = 0 To outputSize - 1
                        outputError(i) = output(i) - targets(sample)(i)
                    Next

                    ' Update output layer
                    For i As Integer = 0 To outputSize - 1
                        For j As Integer = 0 To hiddenSize - 1
                            weights2(j, i) -= learningRate * outputError(i) * hidden(j)
                        Next
                        biases2(i) -= learningRate * outputError(i)
                    Next

                    ' Backpropagate to hidden layer
                    Dim hiddenError(hiddenSize - 1) As Double
                    For i As Integer = 0 To hiddenSize - 1
                        hiddenError(i) = 0
                        For j As Integer = 0 To outputSize - 1
                            hiddenError(i) += outputError(j) * weights2(i, j)
                        Next
                        hiddenError(i) *= (1 - hidden(i) * hidden(i)) ' tanh derivative
                    Next

                    ' Update hidden layer
                    For i As Integer = 0 To hiddenSize - 1
                        For j As Integer = 0 To inputSize - 1
                            weights1(j, i) -= learningRate * hiddenError(i) * inputs(sample)(j)
                        Next
                        biases1(i) -= learningRate * hiddenError(i)
                    Next
                Next
            Next
        End Sub

        Public Sub TrainPolicyGradient(states()(), actions() As Integer, rewards() As Double, batchSize As Integer)
            For sample As Integer = 0 To batchSize - 1
                Dim probabilities = PredictProbabilities(states(sample))
                Dim action = actions(sample)
                Dim reward = rewards(sample)

                ' Calculate gradient
                For i As Integer = 0 To outputSize - 1
                    Dim gradient = probabilities(i) * reward
                    If i = action Then
                        gradient = (probabilities(i) - 1) * reward
                    End If

                    ' Apply gradient (simplified)
                    For j As Integer = 0 To hiddenSize - 1
                        weights2(j, i) -= learningRate * gradient * 0.01 ' Simplified update
                    Next
                Next
            Next
        End Sub

        Public Sub CopyWeights(otherNetwork As DeepQNetwork)
            Array.Copy(otherNetwork.weights1, weights1, weights1.Length)
            Array.Copy(otherNetwork.weights2, weights2, weights2.Length)
            Array.Copy(otherNetwork.biases1, biases1, biases1.Length)
            Array.Copy(otherNetwork.biases2, biases2, biases2.Length)
        End Sub
    End Class

    ' Environment Classes
    Private Class GridWorldEnvironment
        Private width, height As Integer
        Private playerX, playerY As Integer
        Private goalX, goalY As Integer
        Private obstacles As New List(Of Point)()

        Public Sub New(w As Integer, h As Integer)
            width = w
            height = h
            Reset()
        End Sub

        Public Sub Reset()
            playerX = 0
            playerY = 0
            goalX = width - 1
            goalY = height - 1

            obstacles.Clear()
            For i As Integer = 0 To width \ 2
                obstacles.Add(New Point(i * 2, height \ 2))
            Next
        End Sub

        Public Function GetState() As Double()
            Return {playerX / width, playerY / height}
        End Function

        Public Function Step(action As Integer) As Double
            Select Case action
                Case 0 : playerY = Math.Max(0, playerY - 1) ' Up
                Case 1 : playerY = Math.Min(height - 1, playerY + 1) ' Down
                Case 2 : playerX = Math.Max(0, playerX - 1) ' Left
                Case 3 : playerX = Math.Min(width - 1, playerX + 1) ' Right
            End Select

            If IsObstacle(playerX, playerY) Then
                Return -1
            ElseIf playerX = goalX AndAlso playerY = goalY Then
                Return 10
            Else
                Return -0.1
            End If
        End Function

        Public Function IsTerminal() As Boolean
            Return (playerX = goalX AndAlso playerY = goalY) Or IsObstacle(playerX, playerY)
        End Function

        Private Function IsObstacle(x As Integer, y As Integer) As Boolean
            Return obstacles.Any(Function(p) p.X = x And p.Y = y)
        End Function

        Public Sub Render(g As Graphics, width As Integer, height As Integer)
            Dim cellWidth = width \ Me.width
            Dim cellHeight = height \ Me.height

            ' Draw grid
            For x As Integer = 0 To Me.width - 1
                For y As Integer = 0 To Me.height - 1
                    Dim rect As New Rectangle(x * cellWidth, y * cellHeight, cellWidth, cellHeight)

                    If x = goalX And y = goalY Then
                        g.FillRectangle(Brushes.Green, rect)
                    ElseIf IsObstacle(x, y) Then
                        g.FillRectangle(Brushes.Red, rect)
                    Else
                        g.FillRectangle(Brushes.White, rect)
                    End If

                    g.DrawRectangle(Pens.Black, rect)
                Next
            Next

            ' Draw player
            Dim playerRect As New Rectangle(playerX * cellWidth, playerY * cellHeight, cellWidth, cellHeight)
            g.FillRectangle(Brushes.Blue, playerRect)
        End Sub
    End Class

    Private Class CartPoleEnvironment
        Private cartPosition, cartVelocity, poleAngle, poleVelocity As Double
        Private gravity As Double = 9.8
        Private massCart As Double = 1.0
        Private massPole As Double = 0.1
        Private totalMass As Double = massCart + massPole
        Private length As Double = 0.5
        Private poleMassLength As Double = massPole * length
        Private forceMag As Double = 10.0
        Private tau As Double = 0.02

        Public Sub Reset()
            cartPosition = 0
            cartVelocity = 0
            poleAngle = 0.1
            poleVelocity = 0
        End Sub

        Public Function GetState() As Double()
            Return {cartPosition, cartVelocity, poleAngle, poleVelocity}
        End Function

        Public Function Step(action As Integer) As Double
            Dim force = If(action = 0, -forceMag, forceMag)

            Dim cosTheta = Math.Cos(poleAngle)
            Dim sinTheta = Math.Sin(poleAngle)

            Dim temp = (force + poleMassLength * poleVelocity * poleVelocity * sinTheta) / totalMass
            Dim thetaAcc = (gravity * sinTheta - cosTheta * temp) / (length * (4.0 / 3.0 - massPole * cosTheta * cosTheta / totalMass))
            Dim xAcc = temp - poleMassLength * thetaAcc * cosTheta / totalMass

            ' Update state using Euler's method
            cartPosition += tau * cartVelocity
            cartVelocity += tau * xAcc
            poleAngle += tau * poleVelocity
            poleVelocity += tau * thetaAcc

            Dim reward As Double = 1.0

            If Math.Abs(cartPosition) > 2.4 Or Math.Abs(poleAngle) > Math.PI / 4 Then
                reward = -10
            End If

            Return reward
        End Function

        Public Function IsTerminal() As Boolean
            Return Math.Abs(cartPosition) > 2.4 Or Math.Abs(poleAngle) > Math.PI / 4
        End Function

        Public Sub Render(g As Graphics, width As Integer, height As Integer)
            ' Simplified rendering for cart-pole
            Dim scale = Math.Min(width, height) / 5
            Dim cartX = width \ 2 + CInt(cartPosition * scale)
            Dim cartY = height \ 2

            ' Draw cart
            g.FillRectangle(Brushes.Blue, cartX - 20, cartY - 10, 40, 20)

            ' Draw pole
            Dim poleEndX = cartX + CInt(Math.Sin(poleAngle) * length * scale * 2)
            Dim poleEndY = cartY - CInt(Math.Cos(poleAngle) * length * scale * 2)
            g.DrawLine(New Pen(Color.Red, 3), cartX, cartY, poleEndX, poleEndY)
        End Sub
    End Class

    Private Class MountainCarEnvironment
        Private position, velocity As Double
        Private minPosition As Double = -1.2
        Private maxPosition As Double = 0.6
        Private maxVelocity As Double = 0.07
        Private goalPosition As Double = 0.5

        Public Sub Reset()
            position = -0.5
            velocity = 0
        End Sub

        Public Function GetState() As Double()
            Return {position, velocity}
        End Function

        Public Function Step(action As Integer) As Double
            Dim force = action - 1 ' -1, 0, or 1

            velocity += force * 0.001 + Math.Cos(3 * position) * (-0.0025)
            velocity = Math.Max(-maxVelocity, Math.Min(maxVelocity, velocity))
            position += velocity
            position = Math.Max(minPosition, Math.Min(maxPosition, position))

            If position = minPosition And velocity < 0 Then
                velocity = 0
            End If

            Dim reward As Double = -1
            If position >= goalPosition Then
                reward = 10
            End If

            Return reward
        End Function

        Public Function IsTerminal() As Boolean
            Return position >= goalPosition
        End Function

        Public Sub Render(g As Graphics, width As Integer, height As Integer)
            ' Draw mountain
            Dim scaleX = width / (maxPosition - minPosition)
            Dim scaleY = height / 2

            For x As Integer = 0 To width - 1
                Dim worldX = minPosition + x / scaleX
                Dim worldY = Math.Sin(3 * worldX) * 0.45 + 0.55
                Dim screenY = CInt((1 - worldY) * scaleY)

                g.DrawLine(Pens.Black, x, screenY, x, height)
            Next

            ' Draw car
            Dim carX = CInt((position - minPosition) * scaleX)
            Dim worldY = Math.Sin(3 * position) * 0.45 + 0.55
            Dim carY = CInt((1 - worldY) * scaleY)

            g.FillRectangle(Brushes.Red, carX - 5, carY - 5, 10, 10)

            ' Draw goal
            Dim goalX = CInt((goalPosition - minPosition) * scaleX)
            g.DrawLine(New Pen(Color.Green, 3), goalX, 0, goalX, height)
        End Sub
    End Class

    Private Class FrozenLakeEnvironment
        Private width, height As Integer
        Private playerX, playerY As Integer
        Private goalX, goalY As Integer
        Private holes As New List(Of Point)()

        Public Sub New(w As Integer, h As Integer)
            width = w
            height = h
            Reset()
        End Sub

        Public Sub Reset()
            playerX = 0
            playerY = 0
            goalX = width - 1
            goalY = height - 1

            holes.Clear()
            For i As Integer = 1 To width - 2
                holes.Add(New Point(i, height \ 2))
            Next
        End Sub

        Public Function GetState() As Double()
            Return {playerX / width, playerY / height}
        End Function

        Public Function Step(action As Integer) As Double
            Select Case action
                Case 0 : playerY = Math.Max(0, playerY - 1) ' Up
                Case 1 : playerY = Math.Min(height - 1, playerY + 1) ' Down
                Case 2 : playerX = Math.Max(0, playerX - 1) ' Left
                Case 3 : playerX = Math.Min(width - 1, playerX + 1) ' Right
            End Select

            If IsHole(playerX, playerY) Then
                Return -10
            ElseIf playerX = goalX AndAlso playerY = goalY Then
                Return 10
            Else
                Return -0.1
            End If
        End Function

        Public Function IsTerminal() As Boolean
            Return (playerX = goalX AndAlso playerY = goalY) Or IsHole(playerX, playerY)
        End Function

        Private Function IsHole(x As Integer, y As Integer) As Boolean
            Return holes.Any(Function(p) p.X = x And p.Y = y)
        End Function

        Public Sub Render(g As Graphics, width As Integer, height As Integer)
            Dim cellWidth = width \ Me.width
            Dim cellHeight = height \ Me.height

            For x As Integer = 0 To Me.width - 1
                For y As Integer = 0 To Me.height - 1
                    Dim rect As New Rectangle(x * cellWidth, y * cellHeight, cellWidth, cellHeight)

                    If x = goalX And y = goalY Then
                        g.FillRectangle(Brushes.Green, rect)
                    ElseIf IsHole(x, y) Then
                        g.FillRectangle(Brushes.Blue, rect)
                    Else
                        g.FillRectangle(Brushes.White, rect)
                    End If

                    g.DrawRectangle(Pens.Black, rect)
                Next
            Next

            ' Draw player
            Dim playerRect As New Rectangle(playerX * cellWidth, playerY * cellHeight, cellWidth, cellHeight)
            g.FillRectangle(Brushes.Red, playerRect)
        End Sub
    End Class

    ' Random generator
    Private Shared RandomGenerator As New Random()

    ' Application entry point
    Public Shared Sub Main()
        Application.EnableVisualStyles()
        Application.SetCompatibleTextRenderingDefault(False)
        Application.Run(New ReinforcementLearningDemo())
    End Sub
End Class