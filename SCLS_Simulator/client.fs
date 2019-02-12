#light
(*
#r @"..\SCLS\scls.dll";;
#r @"..\SCLS_Parser\bin\Relase\SCLS_Parser.dll";;
#r @".\ZedGraph.dll";;
#r @".\SplashScreen.dll";;
#r @"..\CodeExpressionEvaluator\bin\Relase\CodeExpressionEvaluator.dll";;
#r @"..\SplashScreen\bin\Relase\SplashScreen.dll" 
*)
open System
open System.Windows.Forms
open System.Collections
open System.Collections.Generic
open SCLS
open ZedGraph
open System.IO
open System.Drawing
open System.Drawing.Drawing2D
open System.Drawing.Imaging
open System.ComponentModel
open System.Threading
//open Microsoft..Idioms
open Worker
open Microsoft.FSharp.Math
//open Microsoft..Math.BigInt
open SCLS.Splasher
open System.Drawing;
open System.Timers;

Application.EnableVisualStyles()
Application.SetCompatibleTextRenderingDefault( false );

let mutable mono = false

type viewTermTreeForm =
    class
        inherit System.Windows.Forms.Form
        val mutable treeView : TreeView
        new() = 
            {treeView = new TreeView(Dock = DockStyle.Fill);}
        member x.setTreeView(t:TreeNode) =
            do x.SuspendLayout();
            
            do x.AutoScaleDimensions <- new System.Drawing.SizeF(6.0f, 13.0f);
            do x.AutoScaleMode <- System.Windows.Forms.AutoScaleMode.Font;
            do x.ClientSize <- new System.Drawing.Size(500, 282);
            do x.AutoScroll <- true;
            do x.AutoSize <- true;
            
            do x.FormBorderStyle <- System.Windows.Forms.FormBorderStyle.SizableToolWindow; //FixedDialog
            do x.Name <- "Form3";
            do x.Text <- "StochasticCLS Machine - Term Tree View";
        
            x.treeView <- new TreeView(Dock = DockStyle.Fill);
            x.treeView.Font <- new Font("Courier New", 10.0f, FontStyle.Regular);
            //x.treeView.Sorted <- true
            x.treeView.MouseClick.Add(fun  ( e:MouseEventArgs)  
                                        -> 
                                        match e.Button with 
                                            |  Windows.Forms.MouseButtons.Right ->  x.treeView.ExpandAll();
                                            |  Windows.Forms.MouseButtons.Middle ->  x.treeView.CollapseAll()
                                            | _->()
            )
            x.treeView.Sort();
            x.treeView.Nodes.Add(t) |> ignore
            
            //x.treeView.ExpandAll()
            x.Controls.Clear()
            x.Controls.Add(x.treeView)
            
            x.ResumeLayout()
            x.Refresh()
            //x.Invalidate()      
    end
    
type viewPhaseSpaceForm =
    class
        member x.updateGraphData() =
            x.graph.GraphPane.CurveList <- new CurveList()
            let list = new PointPairList( );
            let myCurve = x.graph.GraphPane.AddCurve( "Phase", list,  System.Drawing.Color.FromArgb(0,1,0),(*rotator.NextSymbol*) SymbolType.None);
            
            
            let Xind = x.checks.CheckedIndices.[0]
            let Yind = x.checks.CheckedIndices.[1]
            
            
            let Xcurve = x.curves.get_Item(Xind)
            
            
            let Ycurve = x.curves.get_Item(Yind)
            
            for i=0 to Xcurve.NPts - 1 do
                let Xpoint = Xcurve.Item(i)
                let X = Xpoint.Y
                let Ypoint = Ycurve.Item(i)
                let Y = Ypoint.Y
                x.graph.GraphPane.CurveList.Item(0).AddPoint(new PointPair(float X, float Y))//, time.ToString())
            done
            x.graph.GraphPane.YAxis.Title.Text <- x.checks.Items.[x.checks.CheckedIndices.[0]].ToString()
            x.graph.GraphPane.XAxis.Title.Text <- x.checks.Items.[x.checks.CheckedIndices.[1]].ToString()
            x.graph.AxisChange(); x.graph.Refresh(); x.graph.Invalidate(); x.Invalidate()
            ()
    
        member x.updatedSelection() =
            match x.checks.CheckedItems.Count 
                with 
                    | 0 | 1 -> ()
                    | 2 -> x.updateGraphData()
                    | n -> 
                   (* | n -> 
                            let temp = Array.zeroCreate n
                            x.checks.CheckedIndices.CopyTo(temp,0)
                            for ind in temp do
                                x.checks.SetItemChecked(ind, false)
                            done
                            x.graph.GraphPane.CurveList <- new CurveList()
                            let list = new PointPairList();
                            let myCurve = x.graph.GraphPane.AddCurve( "Phase", list,  System.Drawing.Color.FromArgb(0,1,0),(*rotator.NextSymbol*) SymbolType.None);
                            x.updateGraphData()*)
                            ()
              
        
        inherit System.Windows.Forms.Form
        val mutable graph : ZedGraph.ZedGraphControl
        val mutable checks : System.Windows.Forms.CheckedListBox;
        val mutable  tableLayoutPanel1 :System.Windows.Forms.TableLayoutPanel;
        val mutable curves: CurveList;
        member this.InitializeComponent() =
            this.tableLayoutPanel1 <- new System.Windows.Forms.TableLayoutPanel();
            this.checks <- new System.Windows.Forms.CheckedListBox();
            this.tableLayoutPanel1.SuspendLayout();
            this.SuspendLayout();
            // 
            // tableLayoutPanel1
            // 
            this.tableLayoutPanel1.ColumnCount <- 1;
            this.tableLayoutPanel1.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, float32 50))|>ignore
            this.tableLayoutPanel1.Controls.Add(this.checks, 0, 1);
            this.tableLayoutPanel1.Controls.Add(this.graph, 0, 0);
            this.tableLayoutPanel1.Dock <- System.Windows.Forms.DockStyle.Fill;
            this.tableLayoutPanel1.Location <- new System.Drawing.Point(0, 0);
            this.tableLayoutPanel1.Name <- "tableLayoutPanel1";
            this.tableLayoutPanel1.RowCount <- 2;
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, float32 85))|>ignore
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, float32 15))|>ignore
            this.tableLayoutPanel1.Size <- new System.Drawing.Size(292, 273);
            this.tableLayoutPanel1.TabIndex <- 0;
            // 
            // checkedListBox1
            // 
            this.checks.Dock <- System.Windows.Forms.DockStyle.Fill;
            this.checks.FormattingEnabled <- true;
            (*this.checkedListBox1.Items.AddRange(new object[] {
            "primo",
            "secondo",
            "ternzo",
            "quartor",
            "r",
            "t",
            "t",
            "y",
            "g",
            "f",
            "sd",
            "we",
            "d",
            "erf"});*)
            this.checks.Location <- new System.Drawing.Point(3, 139);
            this.checks.MultiColumn <- true;
            this.checks.Name <- "checkedListBox1";
            this.checks.Size <- new System.Drawing.Size(286, 124);
            this.checks.TabIndex <- 0;
            // 
            // Form1
            // 
            this.AutoScaleDimensions <- new System.Drawing.SizeF(float32 6, float32 13);
            this.AutoScaleMode <- System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize <- new System.Drawing.Size(440, 790);
            this.Controls.Add(this.tableLayoutPanel1);
            this.tableLayoutPanel1.ResumeLayout(false);
            this.ResumeLayout(false);

        new() as x = 
            {graph =  new ZedGraph.ZedGraphControl  (
                                                    EditButtons = System.Windows.Forms.MouseButtons.Left,
                                                    EditModifierKeys = (  (  System.Windows.Forms.Keys.Alt ||| System.Windows.Forms.Keys.None  )  ),
                                                    IsAutoScrollRange = false,
                                                    IsEnableHEdit = false,
                                                    IsEnableHPan = true,
                                                    IsEnableHZoom = true,
                                                    IsEnableVEdit = false,
                                                    IsEnableVPan = true,
                                                    IsEnableVZoom = true,
                                                    IsPrintFillPage = true,
                                                    IsPrintKeepAspectRatio = true,
                                                    IsScrollY2 = false,
                                                    IsShowContextMenu = true,
                                                    IsShowCopyMessage = true,
                                                    IsShowCursorValues = false,
                                                    IsShowHScrollBar = false,
                                                    IsShowPointValues = false,
                                                    IsShowVScrollBar = false,
                                                    IsSynchronizeXAxes = false,
                                                    IsSynchronizeYAxes = false,
                                                    IsZoomOnMouseCenter = false,
                                                    LinkButtons = System.Windows.Forms.MouseButtons.Left,
                                                    LinkModifierKeys = ( (  System.Windows.Forms.Keys.Alt ||| System.Windows.Forms.Keys.None  ) ),
                                                    Location = new System.Drawing.Point( 12, 12 ),
                                                    Name = "Phase",
                                                    PanButtons = System.Windows.Forms.MouseButtons.Left,
                                                    PanButtons2 = System.Windows.Forms.MouseButtons.Middle,
                                                    PanModifierKeys = (  (  System.Windows.Forms.Keys.Shift ||| System.Windows.Forms.Keys.None  )  ),
                                                    PanModifierKeys2 = System.Windows.Forms.Keys.None,
                                                    ScrollMaxX = float 0,
                                                    ScrollMaxY = float 0,
                                                    ScrollMaxY2 = float 0,
                                                    ScrollMinX = float 0,
                                                    ScrollMinY = float 0,
                                                    ScrollMinY2 = float 0,
                                                    Size = new System.Drawing.Size( 439, 297 ),
                                                    TabIndex = 0,
                                                    ZoomButtons = System.Windows.Forms.MouseButtons.Left,
                                                    ZoomButtons2 = System.Windows.Forms.MouseButtons.None,
                                                    ZoomModifierKeys = System.Windows.Forms.Keys.None,
                                                    ZoomModifierKeys2 = System.Windows.Forms.Keys.None,
                                                    ZoomStepFraction = 0.1,
                                                    IsAntiAlias = true//,
                                                    //SelectButtons = System.Windows.Forms.MouseButtons.Left
                                                    

                                        );
                                        checks = new System.Windows.Forms.CheckedListBox();
                                        tableLayoutPanel1 = new System.Windows.Forms.TableLayoutPanel();
                                        curves = new CurveList()
                                        } 
                                        then
                                                // Set the title and the legends of the plot
                                            x.graph.GraphPane.Title.Text <- "Phase Space";
                                            

                                            x.graph.GraphPane.XAxis.MajorGrid.IsVisible <- true;
                                            x.graph.GraphPane.YAxis.MajorGrid.IsVisible <- true;
                                            x.graph.GraphPane.XAxis.MajorGrid.Color <- Color.LightGray;
                                            x.graph.GraphPane.YAxis.MajorGrid.Color <- Color.LightGray;
                                            
                                            x.graph.Dock <- System.Windows.Forms.DockStyle.Fill

                                            x.graph.AxisChange(); x.graph.Refresh();
                                            x.InitializeComponent();
                               
        member x.Init(numberOfPoints:int, c: CurveList) =
           
            x.SuspendLayout();

            x.AutoScaleDimensions <- new System.Drawing.SizeF(6.0f, 13.0f);
            x.AutoScaleMode <- System.Windows.Forms.AutoScaleMode.Font;
            x.ClientSize <- new System.Drawing.Size(500, 282);
            x.AutoScroll <- true;
            x.AutoSize <- true;
            
            x.FormBorderStyle <- System.Windows.Forms.FormBorderStyle.SizableToolWindow; //FixedDialog
            x.Name <- "Form4";
            x.Text <- "StochasticCLS Machine - Phase Space Graph";
        
            ///todo
            x.graph.GraphPane.CurveList <- new CurveList()
            let list = new RollingPointPairList( numberOfPoints);
            let myCurve = x.graph.GraphPane.AddCurve( "Phase", list,  System.Drawing.Color.FromArgb(0,1,0),(*rotator.NextSymbol*) SymbolType.None);
            
            x.checks.Items.Clear()
            if c.Count > 1 
                then
                //let elements = new ResizeArray<string>();
                for cc in c do 
                    x.checks.Items.Add(cc.Label.Text) |> ignore
                done

                x.checks.SetItemChecked(0,true)
                x.checks.SetItemChecked(1,true)
                
                
                x.graph.GraphPane.YAxis.Title.Text <- x.checks.Items.[0].ToString()
                x.graph.GraphPane.XAxis.Title.Text <- x.checks.Items.[1].ToString()
                //myCurve.AddPoint( new PointPair(0.0, 0.0))
               
                
                //x.Controls.Clear()
                //x.Controls.Add(x.graph)
            
                else ()
            x.checks.SelectedIndexChanged.Add(fun _ -> x.updatedSelection())
            x.curves <- c
            x.ResumeLayout()
            x.Refresh()
            //x.Invalidate()  
            
        member x.AddDate(time:float, X: int64, Y: int64) =
            x.graph.GraphPane.CurveList.Item(0).AddPoint(new PointPair(float X, float Y))//, time.ToString())
            x.graph.AxisChange(); x.graph.Refresh(); x.graph.Invalidate(); x.Invalidate()
        
         member x.AddData(time:float) =
            if x.checks.CheckedIndices.Count >=2 
                then
                let Xind = x.checks.CheckedIndices.[0]
                let Yind = x.checks.CheckedIndices.[1]
                
                
                let Xcurve = x.curves.get_Item(Xind)
                let Xpoint = Xcurve.Item(Xcurve.NPts - 1)
                let X = Xpoint.Y
                
                let Ycurve = x.curves.get_Item(Yind)
                let Ypoint = Ycurve.Item(Ycurve.NPts - 1)
                let Y = Ypoint.Y
                
                x.graph.GraphPane.CurveList.Item(0).AddPoint(new PointPair(float X, float Y))//, time.ToString())
                x.graph.AxisChange(); x.graph.Refresh(); x.graph.Invalidate(); x.Invalidate()
            
    end

type  viewInputFileForm = 
    class
        inherit System.Windows.Forms.Form
        val text : string
        val mutable richTextBox1 : System.Windows.Forms.RichTextBox
        member x.ParseLine( line : string) =
                let r = new System.Text.RegularExpressions.Regex("([ \t{}():;])", System.Text.RegularExpressions.RegexOptions.Compiled);                
                let tokens = r.Split(line);
                for  token in tokens do
                    // Set the tokens default color and font.
                    x.richTextBox1.SelectionColor <- Color.Black;
                    x.richTextBox1.SelectionFont <- new Font("Courier New", 10.0f, FontStyle.Regular);
                    // Check whether the token is a keyword.
                    let keywords = [|"rules"; "term"; "patterns"; "exclude"|];
                    for i = 0 to keywords.Length - 1 do
                        if keywords.[i] = token.Trim() then
                        // Apply alternative color and font to highlight keyword.
                                x.richTextBox1.SelectionColor <- Color.MediumSeaGreen ;
                                x.richTextBox1.SelectionFont <- new Font("Courier New", 10.0f,FontStyle.Bold);
                    done
                    x.richTextBox1.SelectedText <- token;
                done
                x.richTextBox1.SelectedText <- "\n";
                ()
        new() as this = 
            {text = ""; richTextBox1 = new System.Windows.Forms.RichTextBox();}  then   this.richTextBox1.Size <- new System.Drawing.Size(800, 282);
                                                                                        this.Size <-  new System.Drawing.Size(820, 282);
        member this.setText(text:string) =
            //if this.richTextBox1.IsDisposed then this.richTextBox1 <- new System.Windows.Forms.RichTextBox();
            do this.SuspendLayout();
            // 
            // richTextBox1
            // 
            do this.richTextBox1.Dock <- System.Windows.Forms.DockStyle.Fill;
            do this.richTextBox1.Location <- new System.Drawing.Point(500, 200);
            do this.richTextBox1.Name <- "richTextBox1";
            do this.richTextBox1.ScrollBars <- System.Windows.Forms.RichTextBoxScrollBars.ForcedBoth;
            do this.richTextBox1.ReadOnly <- true;
            do this.richTextBox1.Size <- new System.Drawing.Size(820, 282);
            do this.richTextBox1.TabIndex <- 0;
            do this.richTextBox1.Text <- "";
            do this.richTextBox1.Multiline <- true;
            do this.richTextBox1.WordWrap <- false;
            do this.richTextBox1.AcceptsTab <- true;
            do this.richTextBox1.Dock <- DockStyle.Fill;
            do this.richTextBox1.SelectionFont <- new Font("Courier New", 10.0f, FontStyle.Regular);
            do this.richTextBox1.BackColor <- Color.White;
            do this.richTextBox1.SelectionColor <- Color.Yellow;
            //do richTextBox1.richTextBox1.TextChanged += new System.EventHandler(this.richTextBox1_TextChanged);
            // 
            // Form2
            // 
            do this.AutoScaleDimensions <- new System.Drawing.SizeF(6.0f, 13.0f);
            do this.AutoScaleMode <- System.Windows.Forms.AutoScaleMode.Font;
            do this.ClientSize <- new System.Drawing.Size(800, 282);
            do this.AutoScroll <- true;
            do this.AutoSize <- true;
            do this.Controls.Add(this.richTextBox1);
            do this.FormBorderStyle <- System.Windows.Forms.FormBorderStyle.SizableToolWindow; //FixedDialog
            do this.Name <- "Form2";
            do this.Text <- "StochasticCLS Machine - Input File View";
            
            
            let r = new System.Text.RegularExpressions.Regex("\\r\\n|\\r|\\n", System.Text.RegularExpressions.RegexOptions.Compiled);
            let lines = r.Split(text);
            for l in lines do this.ParseLine(l) done
            //this.ParseLine(text)
            
            do this.ResumeLayout(true);
            do this.Refresh()
            ()
        override x.Dispose(disposing:bool) = (x.Visible <- false)
             
    end

type  viewTermForm = 
    class
        inherit System.Windows.Forms.Form
        val mutable richTextBox1 : System.Windows.Forms.RichTextBox
        new() as this = 
            {richTextBox1 = new System.Windows.Forms.RichTextBox();}  then   this.richTextBox1.Size <- new System.Drawing.Size(600, 100);
                                                                             this.Size <-  new System.Drawing.Size(620, 100);
        member x.ParseLine( line : string) =
                let r = new System.Text.RegularExpressions.Regex("([ ])", System.Text.RegularExpressions.RegexOptions.Compiled);                
                let tokens = r.Split(line);
                for  token in tokens do
                    // Set the tokens default color and font.
                    x.richTextBox1.SelectionColor <- Color.Black;
                    x.richTextBox1.SelectionFont <- new Font("Courier New", 10.0f, FontStyle.Regular);
                    // Check whether the token is a keyword.
                    let keywords = [|"loop("; "|"; "("; ")"; "["; "]"|];
                    for i = 0 to keywords.Length - 1 do
                        if keywords.[i] = token.Trim() then
                        // Apply alternative color and font to highlight keyword.
                                x.richTextBox1.SelectionColor <- Color.MediumSeaGreen ;
                                x.richTextBox1.SelectionFont <- new Font("Courier New", 10.0f,FontStyle.Bold);
                    done
                    x.richTextBox1.SelectedText <- token;
                done
                x.richTextBox1.SelectedText <- "\n";
                
                ()
        
        member this.setText(text:string) =
            //if this.richTextBox1.IsDisposed then this.richTextBox1 <- new System.Windows.Forms.RichTextBox();
            do this.SuspendLayout();
            // 
            // richTextBox1
            // 
            do this.richTextBox1.Dock <- System.Windows.Forms.DockStyle.Fill;
            do this.richTextBox1.Location <- new System.Drawing.Point(500, 0);
            do this.richTextBox1.Name <- "richTextBox1";
            do this.richTextBox1.ScrollBars <- System.Windows.Forms.RichTextBoxScrollBars.ForcedBoth;
            do this.richTextBox1.ReadOnly <- true;
            do this.richTextBox1.Size <- new System.Drawing.Size(620, 100);
            do this.richTextBox1.TabIndex <- 0;
            do this.richTextBox1.Text <- "";
            do this.richTextBox1.Multiline <- true;
            do this.richTextBox1.WordWrap <- true;
            do this.richTextBox1.AcceptsTab <- true;
            do this.richTextBox1.Dock <- DockStyle.Fill;
            do this.richTextBox1.SelectionFont <- new Font("Courier New", 10.0f, FontStyle.Regular);
            do this.richTextBox1.BackColor <- Color.White;
            do this.richTextBox1.SelectionColor <- Color.Yellow;

            

            //do richTextBox1.richTextBox1.TextChanged += new System.EventHandler(this.richTextBox1_TextChanged);
            // 
            // Form2
            // 
            do this.AutoScaleDimensions <- new System.Drawing.SizeF(6.0f, 13.0f);
            do this.AutoScaleMode <- System.Windows.Forms.AutoScaleMode.Font;
            do this.ClientSize <- new System.Drawing.Size(600, 100);
            do this.AutoScroll <- true;
            do this.AutoSize <- true;
            do this.Controls.Add(this.richTextBox1);
            do this.FormBorderStyle <- System.Windows.Forms.FormBorderStyle.SizableToolWindow; //FixedDialog
            do this.Name <- "Form2";
            do this.Text <- "StochasticCLS Machine - Term View";
            
            (*
            let r = new System.Text.RegularExpressions.Regex("\\r\\n|\\r|\\n", System.Text.RegularExpressions.RegexOptions.Compiled);
            let lines = r.Split(text);
            for l in lines do this.ParseLine(l) done
            //this.ParseLine(text)
            *)
            //this.richTextBox1.Text <- text
            this.ParseLine(text)
            do this.ResumeLayout(true);
            do this.Refresh()
            ()
        override x.Dispose(disposing:bool) = (x.Visible <- false)
             
    end

type Client() = 
    class    
 
 ///////////////////GUI PORTED F3ROM C# CODE GENERATED BY DESIGNER //////////////////////////////////////////////////////////
       
        let form = new System.Windows.Forms.Form()
               
        let mutable viewInputFileForm =new viewInputFileForm()
        let initViewFileForm(text:string) = 
            viewInputFileForm.setText(text)
            viewInputFileForm.Refresh()
        
        let mutable viewTermTreeForm =new viewTermTreeForm()
        
        
        let mutable viewPhaseSpaceForm =new viewPhaseSpaceForm()
        
        let initViewTermTreeForm(n:Node) = ()
            //viewTermTreeForm.updateTermSimple(n)
            //viewTermTreeForm.Refresh()
        //do viewInputFileForm.setText("")   
        
        //do viewTermTreeForm.updateTermSimple(new Compartment() :> Node)
        //do viewTermTreeForm.Visible <- true
        let viewTermForm = new viewTermForm()
        
        //MENU
        let standardMenuStrip = new  System.Windows.Forms.MenuStrip()
        let fileToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem()
        let actionsToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem()
        let viewToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem()
        let OptionsToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem()
        
        //MENU ITEMS
        let toolStripMenuItem_Open = new System.Windows.Forms.ToolStripMenuItem()
        let toolStripSeparator1 = new System.Windows.Forms.ToolStripSeparator()
        let toolStripSeparator2 = new System.Windows.Forms.ToolStripSeparator()
        let toolStripSeparator3 = new System.Windows.Forms.ToolStripSeparator()
        
        let toolStripMenuItem_Quit = new System.Windows.Forms.ToolStripMenuItem()
        let toolStripMenuItem_Start = new  System.Windows.Forms.ToolStripMenuItem()
        let toolStripMenuItem_Pause = new System.Windows.Forms.ToolStripMenuItem()
        let toolStripMenuItem_Stop = new System.Windows.Forms.ToolStripMenuItem()  
              
        let toolStripMenuItem_Help= new System.Windows.Forms.ToolStripMenuItem()

        let toolStripMenuItem_SwitchLegend = new System.Windows.Forms.ToolStripMenuItem()
        let toolStripMenuItem_SwitchLimit = new System.Windows.Forms.ToolStripMenuItem()
        let toolStripMenuItem_SaveConcentrations = new System.Windows.Forms.ToolStripMenuItem()  
        let toolStripMenuItem_UpdateGraph = new System.Windows.Forms.ToolStripMenuItem()
        let toolStripMenuItem_ShowPhaseSpaceGraph = new System.Windows.Forms.ToolStripMenuItem()
        
        let toolStripMenuItem_SimulateBatch = new System.Windows.Forms.ToolStripMenuItem()
        let toolStripMenuItem_UseConstantSearch = new System.Windows.Forms.ToolStripMenuItem()
        let toolStripMenuItem_UseIncremental = new System.Windows.Forms.ToolStripMenuItem()
        let toolStripMenuItem_UseUltraMemory = new System.Windows.Forms.ToolStripMenuItem()
        
        let toolStripMenuItem_ViewInputFile = new System.Windows.Forms.ToolStripMenuItem()
        //let toolStripMenuItem_ViewTerm = new System.Windows.Forms.ToolStripMenuItem()

        let toolStripMenuItem_About = new System.Windows.Forms.ToolStripMenuItem()
        let toolStripMenuItem_HowTo = new System.Windows.Forms.ToolStripMenuItem()
        
        let toolStripContainer1 = new System.Windows.Forms.ToolStripContainer()
        
        //STATUS STRIP
        let statusStrip1 = new System.Windows.Forms.StatusStrip()
        let toolStripProgressBar1 = new System.Windows.Forms.ToolStripProgressBar()
        let toolStripStatusLabel1 = new System.Windows.Forms.ToolStripStatusLabel()
        let tableLayoutPanel1 = new System.Windows.Forms.TableLayoutPanel()
        
        //LABELS
        let labelFileName = new System.Windows.Forms.Label()
        let labelStepNumber = new System.Windows.Forms.Label()
        //let labelGraphRefreshRate = new System.Windows.Forms.Label()
        let label4 = new System.Windows.Forms.Label ()
        
        //BUTTOMS
        let buttonPlay = new System.Windows.Forms.Button()
        let buttonPause = new System.Windows.Forms.Button()
        let buttonStop = new System.Windows.Forms.Button()
        let openFile_button = new System.Windows.Forms.Button ()
        
        //NUMERICS
        let numericUpDown_StepNumber = new System.Windows.Forms.NumericUpDown()
        //let numericUpDown_GraphRefresh = new System.Windows.Forms.NumericUpDown ()
        let numericUpDown_DataRefresh = new System.Windows.Forms.NumericUpDown ()
        
        //FILEDIALOGS
        let openFileDialog1 = new System.Windows.Forms.OpenFileDialog ()
        let saveFileDialog1 = new System.Windows.Forms.SaveFileDialog ()
        
        //GRAPH
        let mutable zg1 = new ZedGraph.ZedGraphControl()
        
        let fileName_richTextBox1 = new System.Windows.Forms.RichTextBox ()
        do standardMenuStrip.SuspendLayout();
        do toolStripContainer1.ContentPanel.SuspendLayout();
        do toolStripContainer1.TopToolStripPanel.SuspendLayout();
        do toolStripContainer1.SuspendLayout();
        do statusStrip1.SuspendLayout();
        do tableLayoutPanel1.SuspendLayout();
        do ((numericUpDown_StepNumber :> (System.ComponentModel.ISupportInitialize))).BeginInit();
        do ((numericUpDown_DataRefresh :> (System.ComponentModel.ISupportInitialize))).BeginInit();
        do form.SuspendLayout();
        
        //buttonPlay.Image <- Image.FromFile(@".\img\play.jpg") //TO SET AN IMAGE FOR A BOTTON
        
        // 
        // standardMenuStrip
        // 
        do standardMenuStrip.Dock <- System.Windows.Forms.DockStyle.None;
        do standardMenuStrip.Items.Add( fileToolStripMenuItem :> ToolStripItem) |> ignore
        do standardMenuStrip.Items.Add( viewToolStripMenuItem :> ToolStripItem) |> ignore
        do standardMenuStrip.Items.Add( actionsToolStripMenuItem :> ToolStripItem) |> ignore
        do standardMenuStrip.Items.Add( OptionsToolStripMenuItem :> ToolStripItem) |> ignore
        do standardMenuStrip.Items.Add( toolStripMenuItem_Help:> ToolStripItem) |> ignore
        
        
                                                                                         
        do standardMenuStrip.Location <- new System.Drawing.Point(0, 0);
        do standardMenuStrip.Name <- "standardMenuStrip";
        do standardMenuStrip.Size <- new System.Drawing.Size(484, 24);
        do standardMenuStrip.TabIndex <- 0;
        do standardMenuStrip.Text <- "menuStrip1";
        // 
        // fileToolStripMenuItem
        // 
        do fileToolStripMenuItem.DropDownItems.Add( toolStripMenuItem_Open :> ToolStripItem) |> ignore  
        do fileToolStripMenuItem.DropDownItems.Add( toolStripSeparator1 :> ToolStripItem) |> ignore  
        do fileToolStripMenuItem.DropDownItems.Add( toolStripMenuItem_Quit :> ToolStripItem) |> ignore  
                                                                                                  
        do fileToolStripMenuItem.Name <- "fileToolStripMenuItem";
        do fileToolStripMenuItem.Size <- new System.Drawing.Size(37, 20);
        do fileToolStripMenuItem.Text <- "&File";
        // 
        // actionsToolStripMenuItem
        // 
        do actionsToolStripMenuItem.DropDownItems.Add( toolStripMenuItem_Start :> ToolStripItem) |> ignore
        do actionsToolStripMenuItem.DropDownItems.Add( toolStripMenuItem_Pause :> ToolStripItem) |> ignore
        do actionsToolStripMenuItem.DropDownItems.Add( toolStripMenuItem_Stop :> ToolStripItem) |> ignore
        do actionsToolStripMenuItem.DropDownItems.Add( toolStripSeparator2:> ToolStripItem) |> ignore
        do actionsToolStripMenuItem.DropDownItems.Add( toolStripMenuItem_SwitchLegend :> ToolStripItem) |> ignore
        do actionsToolStripMenuItem.DropDownItems.Add( toolStripMenuItem_SaveConcentrations :> ToolStripItem) |> ignore
        do actionsToolStripMenuItem.DropDownItems.Add( toolStripMenuItem_UpdateGraph :> ToolStripItem) |> ignore
        do actionsToolStripMenuItem.DropDownItems.Add( toolStripMenuItem_ShowPhaseSpaceGraph :> ToolStripItem) |> ignore
        
        do actionsToolStripMenuItem.Name <- "actionsToolStripMenuItem";
        do actionsToolStripMenuItem.Size <- new System.Drawing.Size(59, 20);
        do actionsToolStripMenuItem.Text <- "&Actions";
        //
        // OptionsToolStripMenuItem
        //
        //do OptionsToolStripMenuItem.DropDownItems.Add(  toolStripMenuItem_SimulateBatch :> ToolStripItem) |> ignore
        
        do OptionsToolStripMenuItem.DropDownItems.Add( toolStripMenuItem_SwitchLimit :> ToolStripItem) |> ignore
        do OptionsToolStripMenuItem.DropDownItems.Add( toolStripSeparator3:> ToolStripItem) |> ignore
        do OptionsToolStripMenuItem.DropDownItems.Add( toolStripMenuItem_UseConstantSearch :> ToolStripItem) |> ignore
        do OptionsToolStripMenuItem.DropDownItems.Add( toolStripMenuItem_UseIncremental :> ToolStripItem) |> ignore 
        do OptionsToolStripMenuItem.DropDownItems.Add( toolStripMenuItem_UseUltraMemory :> ToolStripItem) |> ignore 
        
        do OptionsToolStripMenuItem.Name <- "OptionsToolStripMenuItem";
        do OptionsToolStripMenuItem.Size <- new System.Drawing.Size(59, 20);
        do OptionsToolStripMenuItem.Text <- "&Options";

        //
        // viewToolStripMenuItem
        //
        do viewToolStripMenuItem.DropDownItems.Add(  toolStripMenuItem_ViewInputFile :> ToolStripItem) |> ignore
        //do viewToolStripMenuItem.DropDownItems.Add(  toolStripMenuItem_ViewTerm :> ToolStripItem) |> ignore
        do viewToolStripMenuItem.Name <- "viewToolStripMenuItem";
        do viewToolStripMenuItem.Size <- new System.Drawing.Size(59, 20);
        do viewToolStripMenuItem.Text <- "&View";
        //do viewToolStripMenuItem.Visible <- false
        
        
        // 
        // helpToolStripMenuItem
        // 
        do toolStripMenuItem_Help.DropDownItems.Add(toolStripMenuItem_About :> ToolStripItem) |> ignore
        do toolStripMenuItem_Help.DropDownItems.Add(toolStripMenuItem_HowTo :> ToolStripItem) |> ignore
        do toolStripMenuItem_Help.Name <- "toolStripMenuItem_Help";
        do toolStripMenuItem_Help.Size <- new System.Drawing.Size(44, 20);
        do toolStripMenuItem_Help.Text <- "&Help";
        // 
        // toolStripMenuItem_Open
        // 
        do toolStripMenuItem_Open.Name <- "toolStripMenuItem_Open";
        do toolStripMenuItem_Open.Size <- new System.Drawing.Size(153, 22);
        do toolStripMenuItem_Open.Text <- "&Open input file and start";
        // 
        // toolStripSeparator1
        // 
        do toolStripSeparator1.Name <- "toolStripSeparator1";
        do toolStripSeparator1.Size <- new System.Drawing.Size(150, 6);
        // 
        // toolStripMenuItem_Quit
        // 
        do toolStripMenuItem_Quit.Name <- "toolStripMenuItem_Quit";
        do toolStripMenuItem_Quit.Size <- new System.Drawing.Size(153, 22);
        do toolStripMenuItem_Quit.Text <- "&Quit";
        // 
        // toolStripMenuItem_Start
        // 
        do toolStripMenuItem_Start.Name <- "toolStripMenuItem_Start";
        do toolStripMenuItem_Start.Size <- new System.Drawing.Size(165, 22);
        do toolStripMenuItem_Start.Text <- "&Start Simulation";
        // 
        // toolStripMenuItem_Pause
        // 
        do toolStripMenuItem_Pause.Name <- "toolStripMenuItem_Pause";
        do toolStripMenuItem_Pause.Size <- new System.Drawing.Size(165, 22);
        do toolStripMenuItem_Pause.Text <- "&Pause Simulation";
        // 
        // toolStripMenuItem_Stop
        // 
        do toolStripMenuItem_Stop.Name <- "toolStripMenuItem_Stop";
        do toolStripMenuItem_Stop.Size <- new System.Drawing.Size(165, 22);
        do toolStripMenuItem_Stop.Text <- "S&top Simulation";
        // 
        // toolStripMenuItem_SwitchLegend
        // 
        do toolStripMenuItem_SwitchLegend.Name <- "toolStripMenuItem_SwitchLegend";
        do toolStripMenuItem_SwitchLegend.Size <- new System.Drawing.Size(165, 22);
        do toolStripMenuItem_SwitchLegend.Text <- "Hide &Legend";
        // 
        // toolStripMenuItem_SwitchLimit
        // 
        do toolStripMenuItem_SwitchLimit.Name <- "toolStripMenuItem_SwitchLimit";
        do toolStripMenuItem_SwitchLimit.Size <- new System.Drawing.Size(165, 22);
        do toolStripMenuItem_SwitchLimit.Text <- "&Time Limit";
        // 
        // toolStripMenuItem_UseConstantSearch
        // 
        do toolStripMenuItem_UseConstantSearch.Name <- "toolStripMenuItem_UseContantSearch";
        do toolStripMenuItem_UseConstantSearch.Size <- new System.Drawing.Size(165, 22);
        do toolStripMenuItem_UseConstantSearch.Text <- "Use &Constant Search";
        do toolStripMenuItem_UseConstantSearch.CheckOnClick <- true
        do toolStripMenuItem_UseConstantSearch.Checked <- false
        // 
        // toolStripMenuItem_UseIncremental
        // 
        do toolStripMenuItem_UseIncremental.Name <- "toolStripMenuItem_UseIncremental";
        do toolStripMenuItem_UseIncremental.Size <- new System.Drawing.Size(165, 22);
        do toolStripMenuItem_UseIncremental.Text <- "Use &Incremental Algorithm";
        do toolStripMenuItem_UseIncremental.CheckOnClick <- true
        do toolStripMenuItem_UseIncremental.Checked <- true
        // 
        // toolStripMenuItem_UseUltraMemory
        // 
        do toolStripMenuItem_UseUltraMemory.Name <- "toolStripMenuItem_UseMemory";
        do toolStripMenuItem_UseUltraMemory.Size <- new System.Drawing.Size(165, 22);
        do toolStripMenuItem_UseUltraMemory.Text <- "Use &Ultra Memory";
        do toolStripMenuItem_UseUltraMemory.CheckOnClick <- true
        do toolStripMenuItem_UseUltraMemory.Checked <- true
        do toolStripMenuItem_UseUltraMemory.Visible <- false
        // 
        // toolStripMenuItem_SimulateBatch
        // 
        do toolStripMenuItem_SimulateBatch.Name <- "toolStripMenuItem_SimulateBatch";
        do toolStripMenuItem_SimulateBatch.Size <- new System.Drawing.Size(165, 22);
        do toolStripMenuItem_SimulateBatch.Text <- "&Simulate Batch";
        do toolStripMenuItem_SimulateBatch.CheckOnClick <- true
        do toolStripMenuItem_SimulateBatch.Checked <- false
        do toolStripMenuItem_SimulateBatch.Visible <- false
        // 
        // toolStripMenuItem_SaveConcentrations
        // 
        do toolStripMenuItem_SaveConcentrations.Name <- "toolStripMenuItem_SaveConcentrations";
        do toolStripMenuItem_SaveConcentrations.Size <- new System.Drawing.Size(165, 22);
        do toolStripMenuItem_SaveConcentrations.Text <- "&Save Concentrations";
        // 
        // toolStripMenuItem_UpdateGraph
        // 
        do toolStripMenuItem_UpdateGraph.Name <- "toolStripMenuItem_UpdateGraph";
        do toolStripMenuItem_UpdateGraph.Size <- new System.Drawing.Size(165, 22);
        do toolStripMenuItem_UpdateGraph.Text <- "&Update Graph";
        // 
        // toolStripMenuItem_ShowPhaseSpaceGraph
        // 
        do toolStripMenuItem_ShowPhaseSpaceGraph.Name <- "toolStripMenuItem_ShowPhaseSpaceGraph";
        do toolStripMenuItem_ShowPhaseSpaceGraph.Size <- new System.Drawing.Size(165, 22);
        do toolStripMenuItem_ShowPhaseSpaceGraph.Text <- "Show &Phase Space Graph";
        // 
        // toolStripSeparator2
        // 
        do toolStripSeparator2.Name <- "toolStripSeparator2";
        do toolStripSeparator2.Size <- new System.Drawing.Size(162, 6);
        // 
        // toolStripMenuItem_About
        // 
        do toolStripMenuItem_About.Name <- "toolStripMenuItem_About";
        do toolStripMenuItem_About.Size <- new System.Drawing.Size(152, 22);
        do toolStripMenuItem_About.Text <- "&About";
        //
        // toolStripMenuItem_HowTo
        // 
        do toolStripMenuItem_HowTo.Name <- "toolStripMenuItem_HowTo";
        do toolStripMenuItem_HowTo.Size <- new System.Drawing.Size(152, 22);
        do toolStripMenuItem_HowTo.Text <- "How &To";
        // 
        // toolStripMenuItem_ViewInputFile
        // 
        do toolStripMenuItem_ViewInputFile.Name <- "toolStripMenuItem_ViewInputFile";
        do toolStripMenuItem_ViewInputFile.Size <- new System.Drawing.Size(165, 22);
        do toolStripMenuItem_ViewInputFile.Text <- "View &Input File";
        do toolStripMenuItem_ViewInputFile.CheckOnClick <- true
        do toolStripMenuItem_ViewInputFile.Checked <- false
        // 
        // toolStripMenuItem_ViewTerm
        // 
        (*do toolStripMenuItem_ViewTerm.Name <- "toolStripMenuItem_ViewTerm";
        do toolStripMenuItem_ViewTerm.Size <- new System.Drawing.Size(165, 22);
        do toolStripMenuItem_ViewTerm.Text <- "View &Term Evolution";
        do toolStripMenuItem_ViewTerm.CheckOnClick <- true
        do toolStripMenuItem_ViewTerm.Checked <- false
        *)
        // 
        // toolStripContainer1
        // 
        // 
        // toolStripContainer1.ContentPanel
        // 
        do toolStripContainer1.ContentPanel.Controls.Add(tableLayoutPanel1);
        do toolStripContainer1.ContentPanel.Controls.Add(statusStrip1);
        do toolStripContainer1.ContentPanel.Size <- new System.Drawing.Size(484, 482);
        do toolStripContainer1.Dock <- System.Windows.Forms.DockStyle.Fill;
        do toolStripContainer1.Location <- new System.Drawing.Point(0, 0);
        do toolStripContainer1.Name <- "toolStripContainer1";
        do toolStripContainer1.Size <- new System.Drawing.Size(484, 506);
        do toolStripContainer1.TabIndex <- 1;
        do toolStripContainer1.Text <- "toolStripContainer1";
        // 
        // toolStripContainer1.TopToolStripPanel
        // 
        do toolStripContainer1.TopToolStripPanel.Controls.Add(standardMenuStrip);
        // 
        // statusStrip1
        // 
        do statusStrip1.Items.Add( toolStripProgressBar1 :> ToolStripItem) |> ignore
        do statusStrip1.Items.Add( toolStripStatusLabel1 :> ToolStripItem) |> ignore
        do statusStrip1.LayoutStyle <- System.Windows.Forms.ToolStripLayoutStyle.HorizontalStackWithOverflow;
        do statusStrip1.Location <- new System.Drawing.Point(0, 460);
        do statusStrip1.Name <- "statusStrip1";
        do statusStrip1.Size <- new System.Drawing.Size(484, 22);
        do statusStrip1.TabIndex <- 1;
        do statusStrip1.Text <- "statusStrip1";
        // 
        // toolStripProgressBar1
        // 
        do toolStripProgressBar1.ForeColor <- System.Drawing.Color.LightGreen;
        do toolStripProgressBar1.Name <- "toolStripProgressBar1";
        do toolStripProgressBar1.Size <- new System.Drawing.Size(250, 16);
        do toolStripProgressBar1.Style <- System.Windows.Forms.ProgressBarStyle.Continuous;
        //do toolStripProgressBar1.Click += new System.EventHandler(toolStripProgressBar1_Click);
        // 
        // toolStripStatusLabel1
        // 
        do toolStripStatusLabel1.Name <- "toolStripStatusLabel1";
        do toolStripStatusLabel1.Size <- new System.Drawing.Size(39, 17);
        do toolStripStatusLabel1.Text <- "";
        //do toolStripStatusLabel1.Click += new System.EventHandler(toolStripStatusLabel1_Click);
        
        //
        // zg1
        //
        do zg1 <- new ZedGraph.ZedGraphControl  (
                                                EditButtons = System.Windows.Forms.MouseButtons.Left,
                                                EditModifierKeys = (  (  System.Windows.Forms.Keys.Alt ||| System.Windows.Forms.Keys.None  )  ),
                                                IsAutoScrollRange = false,
                                                IsEnableHEdit = false,
                                                IsEnableHPan = true,
                                                IsEnableHZoom = true,
                                                IsEnableVEdit = false,
                                                IsEnableVPan = true,
                                                IsEnableVZoom = true,
                                                IsPrintFillPage = true,
                                                IsPrintKeepAspectRatio = true,
                                                IsScrollY2 = false,
                                                IsShowContextMenu = true,
                                                IsShowCopyMessage = true,
                                                IsShowCursorValues = false,
                                                IsShowHScrollBar = false,
                                                IsShowPointValues = false,
                                                IsShowVScrollBar = false,
                                                IsSynchronizeXAxes = false,
                                                IsSynchronizeYAxes = false,
                                                IsZoomOnMouseCenter = false,
                                                LinkButtons = System.Windows.Forms.MouseButtons.Left,
                                                LinkModifierKeys = ( (  System.Windows.Forms.Keys.Alt ||| System.Windows.Forms.Keys.None  ) ),
                                                Location = new System.Drawing.Point( 12, 12 ),
                                                Name = "zg1",
                                                PanButtons = System.Windows.Forms.MouseButtons.Left,
                                                PanButtons2 = System.Windows.Forms.MouseButtons.Middle,
                                                PanModifierKeys = (  (  System.Windows.Forms.Keys.Shift ||| System.Windows.Forms.Keys.None  )  ),
                                                PanModifierKeys2 = System.Windows.Forms.Keys.None,
                                                ScrollMaxX = float 0,
                                                ScrollMaxY = float 0,
                                                ScrollMaxY2 = float 0,
                                                ScrollMinX = float 0,
                                                ScrollMinY = float 0,
                                                ScrollMinY2 = float 0,
                                                Size = new System.Drawing.Size( 439, 297 ),
                                                TabIndex = 0,
                                                ZoomButtons = System.Windows.Forms.MouseButtons.Left,
                                                ZoomButtons2 = System.Windows.Forms.MouseButtons.None,
                                                ZoomModifierKeys = System.Windows.Forms.Keys.None,
                                                ZoomModifierKeys2 = System.Windows.Forms.Keys.None,
                                                ZoomStepFraction = 0.1,
                                                IsAntiAlias = true//,
                                                //SelectButtons = System.Windows.Forms.MouseButtons.Left

                                        )


        // Set the title and the legends of the plot
        do zg1.GraphPane.Title.Text <- "Simulation Result";
        do zg1.GraphPane.YAxis.Title.Text <- "Amount";
        do zg1.GraphPane.XAxis.Title.Text <- "Time";

        do zg1.GraphPane.XAxis.MajorGrid.IsVisible <- true;
        do zg1.GraphPane.YAxis.MajorGrid.IsVisible <- true;
        do zg1.GraphPane.XAxis.MajorGrid.Color <- Color.LightGray;
        do zg1.GraphPane.YAxis.MajorGrid.Color <- Color.LightGray;

        do zg1.AxisChange(); zg1.Refresh();

        // 
        // tableLayoutPanel1
        // 
        do tableLayoutPanel1.ColumnCount <- 3;
        ///
        do tableLayoutPanel1.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 24.79339F)) |> ignore
        do tableLayoutPanel1.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 53.5124F))|> ignore
        do tableLayoutPanel1.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 21.90083F))|> ignore
        ///
        do tableLayoutPanel1.Controls.Add(labelFileName, 0, 0);
        do tableLayoutPanel1.Controls.Add(labelStepNumber, 0, 1);
        //do tableLayoutPanel1.Controls.Add(labelGraphRefreshRate, 0, 2);
        do tableLayoutPanel1.Controls.Add(label4, 0, 2(*3*));
        do tableLayoutPanel1.Controls.Add(buttonPlay, 0, 3);
        do tableLayoutPanel1.Controls.Add(buttonPause, 1, 3);
        do tableLayoutPanel1.Controls.Add(buttonStop, 2, 3);
        do tableLayoutPanel1.Controls.Add(numericUpDown_StepNumber, 1, 1);
        //do tableLayoutPanel1.Controls.Add(numericUpDown_GraphRefresh, 1, 2);
        do tableLayoutPanel1.Controls.Add(numericUpDown_DataRefresh, 1, 2(*3*));
        do tableLayoutPanel1.Controls.Add(zg1, 0, 4);
        do tableLayoutPanel1.Controls.Add(openFile_button, 2, 0);
        do tableLayoutPanel1.Controls.Add(fileName_richTextBox1, 1, 0);
        do tableLayoutPanel1.Dock <- System.Windows.Forms.DockStyle.Fill;
        do tableLayoutPanel1.Location <- new System.Drawing.Point(0, 0);
        do tableLayoutPanel1.Name <- "tableLayoutPanel1";
        do tableLayoutPanel1.RowCount <- 4// 5(*6*);
        
        do tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, float32 30))|> ignore
        do tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, float32 30))|> ignore
        do tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, float32 30))|> ignore
        do tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, float32 30))|> ignore
        do tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, float32 30))|> ignore
        ///
        do tableLayoutPanel1.Size <- new System.Drawing.Size(484, 460);
        do tableLayoutPanel1.TabIndex <- 2;
        // 
        // labelFileName
        // 
        do labelFileName.AutoSize <- true;
        do labelFileName.Dock <- System.Windows.Forms.DockStyle.Fill;
        do labelFileName.Location <- new System.Drawing.Point(3, 0);
        do labelFileName.Name <- "labelFileName";
        do labelFileName.Size <- new System.Drawing.Size(113, 30);
        do labelFileName.TabIndex <- 0;
        do labelFileName.Text <- "File Name";
        do labelFileName.TextAlign <- System.Drawing.ContentAlignment.MiddleCenter;
        //do labelFileName.Click.AddHandler(new System.EventHandler(label1_Click));
        // 
        // labelStepNumber
        // 
        do labelStepNumber.AutoSize <- true;
        do labelStepNumber.Dock <- System.Windows.Forms.DockStyle.Fill;
        do labelStepNumber.Location <- new System.Drawing.Point(3, 30);
        do labelStepNumber.Name <- "labelStepNumber";
        do labelStepNumber.Size <- new System.Drawing.Size(113, 30);
        do labelStepNumber.TabIndex <- 1;
        do labelStepNumber.Text <- "Step Limit";
        do labelStepNumber.TextAlign <- System.Drawing.ContentAlignment.MiddleCenter;
        //do labelStepNumber.Click +<- new System.EventHandler(label2_Click);
        (*// 
        // labelGraphRefreshRate
        // 
        do labelGraphRefreshRate.AutoSize <- true;
        do labelGraphRefreshRate.Dock <- System.Windows.Forms.DockStyle.Fill;
        do labelGraphRefreshRate.Location <- new System.Drawing.Point(3, 60);
        do labelGraphRefreshRate.Name <- "labelGraphRefreshRate";
        do labelGraphRefreshRate.Size <- new System.Drawing.Size(113, 30);
        do labelGraphRefreshRate.TabIndex <- 2;
        do labelGraphRefreshRate.Text <- "Graph Refresh Rate";
        do labelGraphRefreshRate.TextAlign <- System.Drawing.ContentAlignment.MiddleCenter;*)
        // 
        // label4
        // 
        do label4.AutoSize <- true;
        do label4.Dock <- System.Windows.Forms.DockStyle.Fill;
        do label4.Location <- new System.Drawing.Point(3, 90);
        do label4.Name <- "label4";
        do label4.Size <- new System.Drawing.Size(113, 30);
        do label4.TabIndex <- 3;
        do label4.Text <- "Sampling each (it)";
        do label4.TextAlign <- System.Drawing.ContentAlignment.MiddleCenter;
        // 
        // buttonPlay
        // 
        do buttonPlay.Dock <- System.Windows.Forms.DockStyle.Fill;
        do buttonPlay.Location <- new System.Drawing.Point(3, 433);
        do buttonPlay.Name <- "buttonPlay";
        do buttonPlay.Size <- new System.Drawing.Size(113, 24);
        do buttonPlay.TabIndex <- 4;
        do buttonPlay.Text <- "Play";
        do buttonPlay.UseVisualStyleBackColor <- true;
        // 
        // buttonPause
        // 
        do buttonPause.Dock <- System.Windows.Forms.DockStyle.Fill;
        do buttonPause.Location <- new System.Drawing.Point(122, 433);
        do buttonPause.Name <- "buttonPause";
        do buttonPause.Size <- new System.Drawing.Size(252, 24);
        do buttonPause.TabIndex <- 5;
        do buttonPause.Text <- "Pause";
        do buttonPause.UseVisualStyleBackColor <- true;
        //do buttonPause.Click += new System.EventHandler(buttonPause_Click);
        // 
        // buttonStop
        // 
        do buttonStop.Dock <- System.Windows.Forms.DockStyle.Fill;
        do buttonStop.Location <- new System.Drawing.Point(380, 433);
        do buttonStop.Name <- "buttonStop";
        do buttonStop.Size <- new System.Drawing.Size(101, 24);
        do buttonStop.TabIndex <- 6;
        do buttonStop.Text <- "Stop";
        do buttonStop.UseVisualStyleBackColor <- true;
        //do buttonStop.Click += new System.EventHandler(button3_Click);
        // 
        // numericUpDown_StepNumber = STEP NUMBER
        // 
        do tableLayoutPanel1.SetColumnSpan(numericUpDown_StepNumber, 2);
        do numericUpDown_StepNumber.Dock <- System.Windows.Forms.DockStyle.Fill;
        do numericUpDown_StepNumber.Location <- new System.Drawing.Point(122, 33);
        do numericUpDown_StepNumber.Name <- "numericUpDown_StepNumber";
        do numericUpDown_StepNumber.Size <- new System.Drawing.Size(359, 30);
        do numericUpDown_StepNumber.TabIndex <- 7;
        do numericUpDown_StepNumber.Maximum <- new Decimal(999999999);
        do numericUpDown_StepNumber.Minimum <- new Decimal(1);
        do numericUpDown_StepNumber.Value <- new Decimal(1000000);
        
        (*// 
        // numericUpDown_GraphRefresh = GRAPH REFRESH RATE
        // 
        do tableLayoutPanel1.SetColumnSpan(numericUpDown_GraphRefresh, 2);
        do numericUpDown_GraphRefresh.Dock <- System.Windows.Forms.DockStyle.Fill;
        do numericUpDown_GraphRefresh.Location <- new System.Drawing.Point(122, 63);
        do numericUpDown_GraphRefresh.Name <- "numericUpDown_GraphRefresh";
        do numericUpDown_GraphRefresh.Size <- new System.Drawing.Size(359, 20);
        do numericUpDown_GraphRefresh.TabIndex <- 8;
        do numericUpDown_GraphRefresh.Maximum <- new Decimal(999999999);
        do numericUpDown_GraphRefresh.Value <- new Decimal(5)*)
        // 
        // numericUpDown_DataRefresh = DATA REFRESH RATE
        // 
        do tableLayoutPanel1.SetColumnSpan(numericUpDown_DataRefresh, 2);
        do numericUpDown_DataRefresh.Dock <- System.Windows.Forms.DockStyle.Fill;
        do numericUpDown_DataRefresh.Location <- new System.Drawing.Point(122, 93);
        do numericUpDown_DataRefresh.Name <- "numericUpDown_DataRefresh";
        do numericUpDown_DataRefresh.Size <- new System.Drawing.Size(359, 30);
        do numericUpDown_DataRefresh.TabIndex <- 9;
        do numericUpDown_DataRefresh.Maximum <- new Decimal(999999999);
        do numericUpDown_DataRefresh.Minimum <- new Decimal(1);
        do numericUpDown_DataRefresh.Value <- new Decimal(1000)
        // 
        // openFileDialog1
        // 
        do openFileDialog1.FileName <- "";
        do openFileDialog1.Title <- "Select input file and start";
        //do openFileDialog1.FileOk += new System.ComponentModel.CancelEventHandler(openFileDialog1_FileOk);
        do openFileDialog1.FileOk.Add(fun _ -> ())
        // 
        // button1
        // 
        do tableLayoutPanel1.SetColumnSpan(zg1, 3);
        do zg1.Dock <- System.Windows.Forms.DockStyle.Fill;
        do zg1.Location <- new System.Drawing.Point(3, 123);
        do zg1.Name <- "button1";
        do zg1.Size <- new System.Drawing.Size(478, 304);
        do zg1.TabIndex <- 10;
        //do zg1.UseVisualStyleBackColor <- true;
        // 
        // openFile_button
        // 
        do openFile_button.Dock <- System.Windows.Forms.DockStyle.Fill;
        do openFile_button.Location <- new System.Drawing.Point(380, 3);
        do openFile_button.Name <- "openFile_button";
        do openFile_button.Size <- new System.Drawing.Size(101, 24);
        do openFile_button.TabIndex <- 11;
        do openFile_button.Text <- "Open File and Start";
        do openFile_button.Font <- new Font(FontFamily.GenericSansSerif,float32 7)
        do openFile_button.UseVisualStyleBackColor <- true;
        // 
        // fileName_richTextBox1
        // 
        do fileName_richTextBox1.BorderStyle <- System.Windows.Forms.BorderStyle.FixedSingle;
        do fileName_richTextBox1.Dock <- System.Windows.Forms.DockStyle.Fill;
        do fileName_richTextBox1.Location <- new System.Drawing.Point(122, 3);
        do fileName_richTextBox1.Multiline <- false;
        do fileName_richTextBox1.Name <- "fileName_richTextBox1";
        do fileName_richTextBox1.ReadOnly <- true;
        do fileName_richTextBox1.Size <- new System.Drawing.Size(252, 24);
        do fileName_richTextBox1.TabIndex <- 12;
        do fileName_richTextBox1.Text <- "";
        // 
        // Form1
        // 
        do form.AutoScaleDimensions <- new System.Drawing.SizeF(float32 6, float32 13);
        do form.AutoScaleMode <- System.Windows.Forms.AutoScaleMode.Font;
        do form.ClientSize <- new System.Drawing.Size(584, 606);
        do form.Controls.Add(toolStripContainer1);
        do form.MainMenuStrip <-  standardMenuStrip;
        do form.Name <- "Form1";
        do form.Text <- "Stochastic CLS Machine";
        do standardMenuStrip.ResumeLayout(false);
        do standardMenuStrip.PerformLayout();
        do toolStripContainer1.ContentPanel.ResumeLayout(false);
        do toolStripContainer1.ContentPanel.PerformLayout();
        do toolStripContainer1.TopToolStripPanel.ResumeLayout(false);
        do toolStripContainer1.TopToolStripPanel.PerformLayout();
        do toolStripContainer1.ResumeLayout(false);
        do toolStripContainer1.PerformLayout();
        do statusStrip1.ResumeLayout(false);
        do statusStrip1.PerformLayout();
        do tableLayoutPanel1.ResumeLayout(false);
        do tableLayoutPanel1.PerformLayout();
        do ((numericUpDown_StepNumber :> (System.ComponentModel.ISupportInitialize))).EndInit();
        //do ((numericUpDown_GraphRefresh) :> (System.ComponentModel.ISupportInitialize)).EndInit();
        do ((numericUpDown_DataRefresh :> (System.ComponentModel.ISupportInitialize))).EndInit();
        
        do form.ResumeLayout(false);

        let t = Type.GetType ("Mono.Runtime")
        do match t with
                |       null ->(
                                //graph_refresh_rate <- step_number / int numericUpDown_GraphRefresh.Value
                                )
                | _ ->      (
                            /// here a workuround for Mono Runtime :
                            /// Setting the Value property for the numeric up down control while it's
                            /// not visible results in that value not being set
                            numericUpDown_StepNumber.Value <- new Decimal(1000000)
                            numericUpDown_DataRefresh.Value <- new Decimal(1000)
                            /// here set as readonly becouse Mono do not take correct value when edit the inner text box
                            /// may be better to add a Parse of the entered value on on change handler of the inner text box
                            numericUpDown_StepNumber.ReadOnly <- true
                            numericUpDown_DataRefresh.ReadOnly <- true 
                 )
                 
        ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        
        // Add the operations to redraw the GUI at various points
        //let remoteInvoke f = form.BeginInvoke(new MethodInvoker(f)) |> ignore
        
        let mutable step_number =  0.0//int numericUpDown_StepNumber.Value
        let mutable data_refresh_rate = 0.0//int numericUpDown_DataRefresh.Value
        //let mutable graph_refresh_rate = 0//step_number / int numericUpDown_GraphRefresh.Value
        
        
        do step_number <-  float numericUpDown_StepNumber.Value
        do data_refresh_rate <- float numericUpDown_DataRefresh.Value
        
        let mutable worker:Worker.Worker = new Worker.Worker(0.0,0.0,0.0,"",false,false,false)
        let mutable workerThread = new Thread(  start=(fun () -> ()), 
                                                IsBackground = true, 
                                                Priority = ThreadPriority.Lowest)
        let mutable symbols = new SymbolTable()
        
        let mutable patterns: Pattern array = Array.zeroCreate 0
        
        let mutable variables = Set.empty
        
        let mutable exclude = Set.empty
        
        let mutable inputFile = ""
        
        let mutable exclude_count = ref 0
        
        //let mutable model_Option : Model Option = None
        
        let mutable show_legend = true
        
        let mutable show_input= false
        
        let mutable show_term = true
        
        let mutable iteration_limit = true
        
        let mutable simulate_batch = false
        
        // Initialize a color and symbol type rotator
        let rotator = new ColorSymbolRotator()
        
        //let mutable out = new System.Text.StringBuilder()
        
        //let rnd = new System.Random(((int)System.DateTime.Now.Ticks))
        
        let mutable graph_panel_Option : GraphPane Option = None
        
        let updateGraph() =
            zg1.AxisChange(); zg1.Refresh(); zg1.Invalidate(); form.Invalidate()
        
        let addToGraph(time:float, concentrations: int64 array(*, term_string: string*), updateView) = 
            let limit = symbols.Count() - !exclude_count
            for i = 0 to concentrations.Length-1 do
                let curvaCorrente, identifier = 
                    let id = 
                        if i < (if limit = 0 then limit else symbols.Count()) 
                        
                        then symbols.table.[i]
                        else 
                                let pattern_number= 
                                    if symbols.Count() = !exclude_count 
                                        then i
                                        else i-(symbols.Count())    
                                patterns.[pattern_number].name
                    zg1.GraphPane.CurveList.Item(id),id
                if (not (curvaCorrente = null) ) 
                    then curvaCorrente.AddPoint( new PointPair(time, float (concentrations:int64 array).[i], identifier + " at time " + time.ToString() + " = " + (concentrations:int64 array).[i].ToString() (*+ "\n" + term_string*)))
            done
            if updateView then updateGraph()
            
            /////////////////////
            viewPhaseSpaceForm.AddData(time)//,concentrations.[concentrations.Length - 2], concentrations.[concentrations.Length - 1 ])
            /////////////////////
            ()
        
        let updateLegend() = zg1.GraphPane.Legend.IsVisible <- show_legend; 
        
        let initGraph((graphPane:GraphPane),(model : Model ), numberOfPoints:int(*, startConcentrations*) ) =
            viewTermForm.Visible <- false
            variables <- model.variables
            exclude <- model.exclude
            //model_Option <- Some(model)
            symbols <- model.symbols
            patterns <- model.patterns
            graph_panel_Option <- Some(graphPane)
            exclude_count := model.exclude.Count
            //let concentrations = model.term.getConcentrations(symbols.Count())
            graphPane.CurveList <- new CurveList()
            updateLegend()
            //let dataSize = step_number / data_refresh_rate
            for keyVal in model.symbols.table do
                if not (model.exclude.Count = model.symbols.Count()) && not (model.variables.Contains(keyVal.Key)) && not (model.exclude.Contains(keyVal.Key))
                    then
                        ///HERE TO SHOW ONLY NON VARIABLES
                        //let (r,g,b) = (rnd.Next(1,255), rnd.Next(1,255), rnd.Next(1,255))
                        let list = new RollingPointPairList( numberOfPoints);
                        //let list = new PointPairList();
                        //let list = new FilteredPointList([|0.0|],[|0.0|])
                        let myCurve = graphPane.AddCurve( symbols.table.[keyVal.Key].ToString(), list,  rotator.NextColor(*System.Drawing.Color.FromArgb(r,g,b)*),(*rotator.NextSymbol*) SymbolType.None);
                        ()      
            done
            for patt in patterns do
                //let (r,g,b) = (rnd.Next(1,255), rnd.Next(1,255), rnd.Next(1,255))
                let list = new RollingPointPairList( numberOfPoints);
                //let list = new PointPairList();
                //let list = new FilteredPointList([|0.0|],[|0.0|])
                let myCurve = graphPane.AddCurve( patt.name, list, rotator.NextColor(*System.Drawing.Color.FromArgb(r,g,b)*),(*rotator.NextSymbol*) SymbolType.None);
                ()   
            done
            //addToGraph(0.0, startConcentrations(*, model.term.ToString()*) ,false )
            updateGraph()
            
            ////
            viewPhaseSpaceForm.Init(numberOfPoints, zg1.GraphPane.CurveList)
            viewPhaseSpaceForm.Visible <- true
            ///
            ()
                
        let initGuiState() = 
            buttonPlay.Enabled <- false
            buttonPause.Enabled <- false
            buttonStop.Enabled <- false
            numericUpDown_StepNumber.Enabled <- true
            //if iteration_limit then numericUpDown_GraphRefresh.Enabled <- true
            numericUpDown_DataRefresh.Enabled <- true
            toolStripMenuItem_Start.Enabled <- false
            toolStripMenuItem_Pause.Enabled <- false
            toolStripMenuItem_Stop.Enabled <- false
            toolStripMenuItem_ViewInputFile.Visible <- true//false
            toolStripMenuItem_SaveConcentrations.Enabled <- true
            toolStripMenuItem_UpdateGraph.Enabled <- true
            ()
        
        let switchGuiState() =
            let mutable fileName = ""
            //let graph_refresh_rate =ref  0// if iteration_limit then step_number / int numericUpDown_GraphRefresh.Value else data_refresh_rate * 1000

            let step_number =  float numericUpDown_StepNumber.Value
            let data_refresh_rate = if iteration_limit then float numericUpDown_DataRefresh.Value else (float numericUpDown_DataRefresh.Value) / (1000.0)
            let working_dir = System.IO.Directory.GetCurrentDirectory();
            //Console.WriteLine(step_number.ToString() + " " + data_refresh_rate.ToString() )
             
            if (openFileDialog1.ShowDialog() = System.Windows.Forms.DialogResult.OK) 
                then 
                    workerThread.Abort();
                    System.GC.Collect()
                    System.GC.WaitForPendingFinalizers();
                    fileName <- openFileDialog1.FileName;
                    fileName_richTextBox1.Text <- fileName;
                    viewToolStripMenuItem.Visible <- true
            /// Create and configure the worker automata, ready to be placed
            /// onto a thread.
            try
            System.IO.Directory.SetCurrentDirectory(working_dir)
            let useConstant, useIncremental, useUltraMemory = toolStripMenuItem_UseConstantSearch.Checked,toolStripMenuItem_UseIncremental.Checked, toolStripMenuItem_UseUltraMemory.Checked 
            if iteration_limit 
                then worker <- new Worker.Worker( step_number, 0.0(*, !graph_refresh_rate*) , data_refresh_rate,fileName,useConstant,useIncremental,useUltraMemory)
                else worker <- new Worker.Worker( -1.0, step_number(*, !graph_refresh_rate*), data_refresh_rate,fileName,useConstant,useIncremental,useUltraMemory)
            let mutable symbols = new SymbolTable()

            // Create and configure but do not start the worker thread. 
            // IsBackground ensures that thread will not inhibit application exit
            workerThread <- new Thread(     start=(fun () -> worker.StartAsync()), 
                                            IsBackground = true, 
                                            Priority = ThreadPriority.Lowest)
            buttonPlay.Enabled <- true
            buttonPause.Enabled <- true
            buttonStop.Enabled <- true
            numericUpDown_StepNumber.Enabled <- false
            //numericUpDown_GraphRefresh.Enabled <- false
            numericUpDown_DataRefresh.Enabled <- false
            toolStripMenuItem_Start.Enabled <- true
            toolStripMenuItem_Pause.Enabled <- true
            toolStripMenuItem_Stop.Enabled <- true
            //toolStripMenuItem_Parameters.Enabled <- true
            
            // This callback is invoked by the worker on the GUI thread
            // when the game finishes early, which case the menu items need to
            // be reconfigured.
            worker.FinishedEarly.Add(fun () 
                                        ->  (
                                            toolStripStatusLabel1.Text <- "Done" ;//+ toolStripStatusLabel1.Text.Substring(4) ; 
                                            toolStripProgressBar1.Value <- 100; 
                                            updateGraph() 
                                            initGuiState()
                                            ) )
        
        
            // This is the refresh operation that will be invoked by the worker. 
            worker.Updates.Add(fun ((time, concentrations:int64 array) (*,term_string*), updateView, iterationCounter) 
                                    -> (
                                        let percent =   if iteration_limit  
                                                            then int ((float iterationCounter/float step_number) * 100.0); 
                                                            else let x = int ((float time/float step_number) * 100.0) in if x>100 then 100 else x
                                        toolStripStatusLabel1.Text <- (percent).ToString() + "% - it : " + iterationCounter.ToString() + " - time : " + ((time.ToString()).Substring(0,Math.Min(8,(time.ToString()).Length))); 
                                        toolStripProgressBar1.Value <- percent
                                        addToGraph(time,concentrations(*,term_string*),updateView && not simulate_batch)  
                                        )
                                    )
            worker.InitGraph.Add(fun (model, (*time, concentrations:int64 array), *)input:string )
                                    ->  initGraph(zg1.GraphPane,model, int ( step_number / data_refresh_rate) + 1(*, concentrations*));
                                        inputFile <- input
                                        //(viewInputFileForm.Controls.[0]  :?>  (System.Windows.Forms.RichTextBox)).Text <- inputFile;
                                        initViewFileForm(inputFile)
                                        //initViewTermTreeForm((model.term :> Node))
                                        //SplashForm.CloseSplash();
                                        
                                        
                                )

            worker.Error.Add(fun (errorString) -> MessageBox.Show(errorString) |> ignore )
            
            
            
            worker.TermUpdates.Add(fun (termStringRepresentation, (termTreeView:TreeNode)) -> 
                                        viewTermForm.setText(termStringRepresentation); viewTermForm.Visible <- true
                                        viewTermTreeForm.setTreeView(termTreeView); viewTermTreeForm.Visible <- true
                                  )
            
            workerThread.Start()
            with
                | e -> MessageBox.Show(e.Message) |> ignore
            
            (worker)
        
        let writeHtmlOutput(tex:StreamWriter) = 
            let mutable output = new Text.StringBuilder();
            /// initialize output and write the header 
            output.Append("<html><table  border=1><tr><th>time</th>")|>ignore
            for keyVal in symbols.table do
                    if not (!exclude_count = symbols.Count()) && not (variables.Contains(keyVal.Key)) && not (exclude.Contains(keyVal.Key))
                        then
                            output.Append("<th>" + symbols.table.[keyVal.Key].ToString()+ "</th>") |> ignore   
                        else ()
            done
            for patt in patterns do
                 output.Append("<th>" + patt.name + "</th>") |> ignore
            done
            output.Append("</tr>") |> ignore
            
            let curves = graph_panel_Option.Value.CurveList
            
            for i = 0 to (curves.Item(0)).NPts - 1(*number of point in this simulation*) do
                output.Append("<tr>") |> ignore
                if i = 0 
                    then
                        output.Append("<td>0,0000000</td>") |> ignore
                    else
                        let some_curve = (curves.Item(0))
                        let time = some_curve.Item(i).X
                        output.Append("<td>" + (time.ToString())+"</td>") |> ignore
                        
                for keyVal in symbols.table do
                    if not (!exclude_count = symbols.Count()) && not (variables.Contains(keyVal.Key)) && not (exclude.Contains(keyVal.Key))
                        then
                            output.Append("<td>" + curves.get_Item(keyVal.Value).Item(i).Y.ToString()  + "</td>") |> ignore
                done
                for patt in patterns do
                    output.Append("<td>" + curves.get_Item(patt.name).Item(i).Y.ToString() + "</td>") |> ignore
                done
                
                output.Append("</tr>") |> ignore
                
                tex.Write(output.ToString());
                output <- new Text.StringBuilder()
            
            done
            tex.Write("</table></html>")
            

        
        let writeTextOutput(tex:StreamWriter) = 
            /// here assume that each column take exactly 10 characters
            
            /// initialize output and write the header 
            let mutable output = new System.Text.StringBuilder("\t\t\t\t\t\t\t\t\t\t")
            for keyVal in symbols.table do
                    if not (!exclude_count = symbols.Count()) && not (variables.Contains(keyVal.Key)) && not (exclude.Contains(keyVal.Key))
                        then
                                let value = let v = (symbols.table.[keyVal.Key].ToString()) in v.Substring( 0, (Math.Min(10, v.Length)))  ;
                                output.Append(value) |> ignore
                                for i = 0 to 8 - value.Length do
                                    output.Append(" ") |> ignore
                                done
                                if value.Length < 6 then output.Append("\t") |> ignore
                        else ()
            
            done
            for patt in patterns do
                let value =  (patt.name).Substring( 0,  (Math.Min(10, patt.name.Length)))  ;
                output.Append(value) |> ignore
                for i = 0 to 8 - value.Length do
                    output.Append(" ") |> ignore
                done
                if value.Length < 6 then output.Append("\t") |> ignore
            done
            output.Append("\n") |> ignore
            
            let curves = graph_panel_Option.Value.CurveList
            
            for i = 0 to (curves.Item(0)).NPts - 1(*number of point in this simulation*) do
            
                if i = 0 
                    then
                        output.Append("0,0000000\t\t\t\t\t\t") |> ignore
                    else
                        let some_curve = (curves.Item(0))
                        let time = some_curve.Item(i).X
                        output.Append((time.ToString()).Substring(0,Math.Min(9,(time.ToString()).Length)) + "\t\t\t\t\t\t") |> ignore
                        
                for keyVal in symbols.table do
                    if not (!exclude_count = symbols.Count()) && not (variables.Contains(keyVal.Key)) && not (exclude.Contains(keyVal.Key))
                        then
                            let value = let v = curves.get_Item(keyVal.Value).Item(i).Y.ToString() in  v.Substring(0, (Math.Min(10, v.Length)) )
                            output.Append(value) |> ignore
                            for i = 0 to 8 - value.Length do
                                output.Append(" ") |> ignore
                            done
                            if value.Length < 6 then output.Append("\t") |> ignore

                done
                for patt in patterns do
                    let value = let v = curves.get_Item(patt.name).Item(i).Y.ToString() in v.Substring( 0, (Math.Min(10, v.Length)))
                    output.Append(value) |> ignore
                    for i = 0 to 8 - value.Length do
                        output.Append(" ") |> ignore
                    done
                    if value.Length < 6 then output.Append("\t") |> ignore     
                done
                
                tex.Write(output.Append("\n").ToString());
                output <- new Text.StringBuilder()
            
            done
        
        let saveOutput() =
            let mutable outFileName = "out.txt"
            let mutable fileType = 0
            saveFileDialog1.Filter <- "Text files (*.txt)|*.txt|" + //1
                                      "Html file (*.html)|*.html|" + //2
                                      "Spreadsheet (*.xls) | *.xls" //+ //3  
                                      //"All files (*.*)|*.*" //4

            let working_dir = System.IO.Directory.GetCurrentDirectory();
            if (saveFileDialog1.ShowDialog() = System.Windows.Forms.DialogResult.OK) 
                then
                    outFileName <- saveFileDialog1.FileName;
                    fileType <- saveFileDialog1.FilterIndex
            let (t:FileInfo) = new FileInfo(outFileName);
            let  (Tex:StreamWriter) =t.CreateText();
            match fileType with                             ///HERE MONO WRONG !!! TODO a workaround !
                | 1 -> writeTextOutput(Tex)
                | 2 -> writeHtmlOutput(Tex)
                | 3 -> writeHtmlOutput(Tex)
                | _ -> ()
            Tex.Close();
            System.IO.Directory.SetCurrentDirectory(working_dir)
            ()
        
        let switchLegend() = 
            show_legend <- not show_legend
            if show_legend  then toolStripMenuItem_SwitchLegend.Text <- "Hide &Legend";
                            else toolStripMenuItem_SwitchLegend.Text <- "Show &Legend";
            updateLegend()
            updateGraph()
        
        let switchLimit() = 
            iteration_limit <- not iteration_limit
            if iteration_limit then    toolStripMenuItem_SwitchLimit.Text <- "&Time Limit";
                                       label4.Text <- "Sampling each (it)";
                                       labelStepNumber.Text <- "Step Limit"; 
                                       numericUpDown_StepNumber.Value <- new Decimal(1000000)
                                       numericUpDown_DataRefresh.Value <- new Decimal(1000)
                                        
                                else   toolStripMenuItem_SwitchLimit.Text <- "&Iteration number Limit";
                                       labelStepNumber.Text <- "Time Limit";
                                       label4.Text <- "Sampling each (ms)";
                                       //numericUpDown_GraphRefresh.Enabled <- false 
                                       numericUpDown_StepNumber.Value <- new Decimal(10)
                                       numericUpDown_DataRefresh.Value <- new Decimal(50)
                                       
            updateGraph()
        
        let switchViewInputFile() = 
            show_input <- not show_input
            if show_input 
                then    //initViewFileForm(inputFile)
                        viewInputFileForm.Show()
                else    viewInputFileForm.Visible <- false
            ()
        
        (*
        let switchViewTerm() = 
            show_term <- not show_term
            if show_term 
                then    //initViewFileForm(inputFile)
                        viewTermForm.Show()
                else    viewTermForm.Visible <- false
            ()
            *)
        
        let switchSimulateBatch() = ()
        
        let showAbout() = (MessageBox.Show("Stochastic CLS Machine\nv.1.01\n\nA stochastic simulator for Calculus of Looping Sequence in F# \n\n \n Copiright 2007 - Dipartimento di Informatica, Università di Pisa, Italy \n\n - http://www.di.unipi.it/~milazzo/papers/milazzo-phd-thesis.pdf \n http://www.di.unipi.it/~milazzo/biosims/ \n- http://research.microsoft.com//.aspx", "About") |> ignore )
        
        let showHowTo() = (MessageBox.Show("First set the parameters\nand then select the input file to start the simulation\n\nOn the graph use shift to pan and wheel to zoom", "..how to..") |> ignore )
        
        let stopSimulation() =
            workerThread.Abort();
            GC.Collect()
            GC.WaitForPendingFinalizers()
            initGuiState();
            updateGraph();
            
        let sendPause() = (worker.StopAsync;)
        let sendStart() = (viewTermForm.Visible <- false;  viewTermTreeForm.Visible <- false ; worker.StartAsync();)
        let sendReset() = (worker.ResetAsync();)
        let sendResume() = (worker.RunAsync();)
        let sendStep() = (worker.StepAsync();)
        let sendRun() = (worker.RunAsync();)
        
        //CallBacks
        do toolStripMenuItem_Quit.Click.Add(fun _ -> workerThread.Abort(); form.Close(); System.Environment.Exit(0); )
        do toolStripMenuItem_Start.Click.Add(fun _ -> sendStart() )
        do toolStripMenuItem_Pause.Click.Add(fun _ -> sendPause() (); )
        
        do toolStripMenuItem_SwitchLegend.Click.Add(fun _ -> switchLegend() )
        do toolStripMenuItem_SwitchLimit.Click.Add(fun _ -> switchLimit() )
        do toolStripMenuItem_SaveConcentrations.Click.Add(fun _ -> saveOutput() )
        do toolStripMenuItem_UpdateGraph.Click.Add(fun _ -> updateGraph() )
        
        do toolStripMenuItem_ViewInputFile.Click.Add(fun _ -> switchViewInputFile() )
        //do toolStripMenuItem_ViewTerm.Click.Add(fun _ -> switchViewTerm() )
        
        do toolStripMenuItem_SimulateBatch.Click.Add(fun _ -> simulate_batch <- not simulate_batch )
        
        do toolStripMenuItem_About.Click.Add(fun _ -> showAbout())
        do toolStripMenuItem_HowTo.Click.Add(fun _ -> showHowTo())
        
        do buttonStop.Click.Add(fun _ ->  stopSimulation())
        do buttonPlay.Click.Add(fun _ -> sendRun(); viewTermForm.Visible <- false;  viewTermTreeForm.Visible <- false )
        do buttonPause.Click.Add(fun _ -> sendPause() () )
        
        do openFile_button.Click.Add(fun _ ->  worker <- switchGuiState();  viewTermForm.Visible <- false;  viewTermTreeForm.Visible <- false; if mono then openFile_button.Visible <- false else ();)
        
        do viewInputFileForm.Closing.Add(fun _ ->  
            toolStripMenuItem_ViewInputFile.Checked <- false; show_input <- false; )
        
        (*do viewInputFileForm.Closing.Add(fun _ ->  
            toolStripMenuItem_ViewTerm.Checked <- false; show_term <- false; )
        *)    
        do zg1.MouseClick.Add(
        
                                fun  ( e:MouseEventArgs)  
                                    -> 
                                        let mutable onlegend = false
                                        let pt = new PointF(float32 e.X, float32 e.Y )
                                        let index = ref 0
                                        let o,curve  = ref(new obj()),ref( new LineItem("") :> CurveItem)
                                        let grap =  (zg1.CreateGraphics());
                                        let nearestCurve = curve
                                        if zg1.GraphPane.FindNearestObject(pt, grap, o, index)
                                            then ()//print_any((!o).ToString() + " " + (!o).GetType().ToString() ))
                                            else ()
                                        if  (!o <> null) && (!o :? Legend) 
                                            then
                                                let legend = zg1.GraphPane.Legend;
                                                if ( legend <> null )
                                                then
                                                    if ( !index >= 0 )
                                                        then    
                                                            try
                                                                nearestCurve :=( zg1.GraphPane.CurveList.get_Item(!index)) 
                                                                onlegend <- true
                                                            with
                                                                _ -> ()
                                                
                                            
                                            else 
                                                if ( !o :? CurveItem )
                                                    then nearestCurve := (!o :?> CurveItem) 
                                                    (*else 
                                                    if ( !o :? XAxis) || ( !o :? YAxis)
                                                        then   //if ( !o :? XAxis) then (!o :?> XAxis).IsVisible  <- not (!o :?> XAxis).IsVisible 
                                                               //else (!o :?> YAxis).IsVisible  <- not (!o :?> YAxis).IsVisible 
                                                               if ( !o :? XAxis) 
                                                                    then    if zg1.GraphPane.XAxis.Type = AxisType.Log then zg1.GraphPane.XAxis.Type<-AxisType.Linear
                                                                            else zg1.GraphPane.XAxis.Type<-AxisType.Log
                                                               else if zg1.GraphPane.YAxis.Type = AxisType.Log then zg1.GraphPane.YAxis.Type<-AxisType.Linear
                                                                    else zg1.GraphPane.YAxis.Type<-AxisType.Log
                                                      *)         
                                                                
                                        if  !nearestCurve <> null 
                                            then
                                                //(!nearestCurve).IsVisible <- false;
                                                //nearestCurve.Color = Color.Green;
                                                if ( !nearestCurve :? LineItem )
                                                    then  match e.Button, onlegend with
                                                            | Windows.Forms.MouseButtons.Left, true ->   ( !nearestCurve :?> LineItem).Line.Width <- (  !nearestCurve :?> LineItem).Line.Width + 3.0f
                                                            | Windows.Forms.MouseButtons.Middle, true ->   ( !nearestCurve :?> LineItem).Line.Width <- (  !nearestCurve :?> LineItem).Line.Width - 3.0f
                                                            | Windows.Forms.MouseButtons.Left, false ->   if ( !nearestCurve :?> LineItem).Symbol.Type = SymbolType.None 
                                                                                                                            then ( !nearestCurve :?> LineItem).Symbol.Type <-rotator.NextSymbol
                                                                                                                            else ( !nearestCurve :?> LineItem).Symbol.Type <- SymbolType.None
                                                            | Windows.Forms.MouseButtons.Middle, false ->   ( !nearestCurve :?> LineItem).Color <-rotator.NextColor
                                                            
                                                            | _ -> ()

                                        zg1.Invalidate();
                                       
                            )

        do form.Visible <- true
        do initGuiState()
        do form.Activate()
        //HOW TO MSG BOX
        //do MessageBox.Show("First set the parameters\nand then select input file to start the simulation\n\nOn graph use shift to pan and wheel to zoom", "..how to..") |> ignore

        member x.Form = form
        //member x.Dececk = toolStripMenuItem_ViewInputFile.Checked <- false

end
(*
///here select interface in base of framework found
let t = Type.GetType ("Mono.Runtime")
in match t with
    | null ->(
                 //Console.WriteLine("MS Framework")
                 let client = new Client()
                 ()
                 )
    | _ ->      (
                 //Console.WriteLine("Mono Framework")
                 let client = new batch.BatchClient()
                 ()
                 )*)
                 

let t = Type.GetType ("Mono.Runtime")
in match t with
    | null ->()
    | _ -> mono <- true

//[<STAThread>]   
do if not mono then SplashForm.StartSplash(0, Color.FromArgb(128, 128, 128)) else (); 
let client = new Client()
//System.Threading.Thread.Sleep(500);
// close the splash screen
if not mono then SplashForm.CloseSplash(); else ()

// Run the main code. The attribute marks the startup application thread as "Single 
// Thread Apartment" mode, which is necessary for GUI applications. 
[<STAThread>]    
do Application.Run(client.Form)
