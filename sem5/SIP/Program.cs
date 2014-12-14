using System;
using QuickGraph;
using System.Diagnostics;

using System.Linq;
using System.Collections.Generic;

namespace SIP
{
	using Graph = UndirectedGraph<Vertex, Edge>;
	using WeightedGraph = UndirectedGraph<WeightedVertex, WeightedEdge>;

	class MainClass
	{

		class TestRun {

			public TestRun(int runCount, Graph q, Graph g) {
				run (runCount, q, g); 
			}

			public TestRun(Graph q, Graph g) : this (1, q, g) {}

			void run(int runCount, Graph q, Graph g) {
				long sumTimeQuick = 0;
				long sumTimeUllman = 0;
				var seq = SEQ.BuildSEQ (q, g);
				Match = true;

				for (int runNumber = 0; runNumber < runCount; runNumber++) {
					var watch = Stopwatch.StartNew();
					var resultQuick = new QuickSI (seq, g).Result;
					watch.Stop();
					var ms = watch.ElapsedMilliseconds;
					sumTimeQuick += ms;

					watch = Stopwatch.StartNew ();
					var resultUllman = new UllmanSIP (q, g).Result;
					watch.Stop ();
					ms = watch.ElapsedMilliseconds;
					sumTimeUllman += ms;

					if (resultQuick ^ resultUllman)
						Match = false;
				}

				var avgTimeQuick = sumTimeQuick / runCount;
				var avgTimeUllman = sumTimeUllman / runCount;
				TimeQuickSI = avgTimeQuick;
				TimeUllman = avgTimeUllman;
			}

			public bool Match { get; set; }
			public long TimeQuickSI { get; set; }
			public long TimeUllman { get; set; }

			public string ToString (int vCount)
			{
				return (!Match) ? 
					(vCount + " : mismatch") : 
					(vCount + " :\t\t" + TimeUllman + "\t\t" + TimeQuickSI); 
			}
		}

		static void FixedDataTest(int amountOfSteps,
		                          int dataVertexCount,
		                          int dataEdgePercentage,
		                          int startPatternSize,
		                          int patternEdgePercantage,
		                          int patternStepGrowth) {
			var patternVertexCount = startPatternSize;
			//var dataGraph = RandomGraph.GenerateGraph ()
		}

		static void FixedPatternTest(int amountOfSteps, 
		                             int patternVertexCount, 
		                             int patternEdgePercentage, 
		                             int dataGraphStepGrowth) {
			var vCountData = patternVertexCount;
			var q = RandomGraph.GenerateGraph (patternVertexCount, patternEdgePercentage);
			for (int step = 0; step < amountOfSteps; step++) {
				var extend = ((vCountData * dataGraphStepGrowth) / 100);
				var g = RandomGraph.ExtendGraph (q, extend, 30);
				var test = new TestRun (5, q, g);
				vCountData += extend;
				print (test.ToString (vCountData)); 
			}
		}

		public static void Main (string[] args)
		{
			for (int patternSize = 4; patternSize <= 20; patternSize+=2) {
				//print ("Pattern size = " + patternSize);
				FixedPatternTest (10, 8, 50, 10);
			}

			//var q = RandomGraph.OneVertexGraph ();
			/*
			var g1 = RandomGraph.ExtendGraph (q, 4, 0);
			var g2 = RandomGraph.ExtendGraph (q, 4, 100);
			var g3 = RandomGraph.ExtendGraph (q, 4, 40);
			printGraph (q);
			printGraph (g1);
			printGraph (g2);
			printGraph (g3);
			*/

			/*
			for (int vertexCount = 5; vertexCount <= 100; vertexCount += 5) {
				int vCount = vertexCount;
				print ("-----------\t" + vCount + "\t------------");
				for (int p = 0; p <= 100; p += 10) {
					int sumEdgesCount = 0;
					int percent = p;
					int testCount = 100;
					int expectedCount = vCount * (vCount - 1) * percent / 200;
					for (int i = 0; i < testCount; i++) {
						var g = RandomGraph.ExtendGraph (q, vCount - 1, percent);
						int eCount = g.EdgeCount;
						sumEdgesCount += eCount;
					}
					int avgEdgeCount = sumEdgesCount / testCount;
					print (percent + "% : " + expectedCount + " vs. " + avgEdgeCount); 
				}
			}
			*/
			/*
			using (System.IO.StreamWriter file = new System.IO.StreamWriter(@"time.txt")) file.WriteLine("");
			using (System.IO.StreamWriter file = new System.IO.StreamWriter(@"vcount.txt")) file.WriteLine("");
			using (System.IO.StreamWriter file = new System.IO.StreamWriter(@"time.txt", true))
			{
				file.WriteLine(ms);
			}
			using (System.IO.StreamWriter file = new System.IO.StreamWriter(@"vcount.txt", true))
			{
				file.WriteLine(vCount);
			}
			*/
			//print (vCount + " -> " + succ + "/" + count + "  --  " + avgTime + "ms");
		}

		static void print(String s) {
			Console.WriteLine (s);
		}

		static void printGraph(Graph q) {
			print ("Vertex count = " + q.VertexCount);
			foreach (var v in q.Vertices) {
				print(v.ToString ()); 
			}
			print ("Edge count = " + q.EdgeCount);
			foreach (var e in q.Edges) {
				print(e.ToString ()); 
			}
		}

		static void test2() {
			var q = new Graph(false);
			Vertex[] qvs = { 
				new Vertex (0, "A"),
				new Vertex (1, "B"),
				new Vertex (2, "B"),
				new Vertex (3, "C")
			};
			Edge[] qes = {
				new Edge (qvs[0], qvs[1]),
				new Edge (qvs[0], qvs[2]),
				new Edge (qvs[0], qvs[3])
			};
			q.AddVertexRange (qvs);
			q.AddEdgeRange (qes);

			var g = new Graph(false);
			Vertex[] gvs = { 
				new Vertex (0, "C"),
				new Vertex (1, "A"),
				new Vertex (2, "A"),
				new Vertex (3, "A"),
				new Vertex (4, "A"),
				new Vertex (5, "B"),
				new Vertex (6, "B"),
				new Vertex (7, "B"),
				new Vertex (8, "B")
			};
			Edge[] ges = {
				new Edge (gvs[0], gvs[1]),
				new Edge (gvs[0], gvs[2]),
				new Edge (gvs[0], gvs[3]),
				new Edge (gvs[0], gvs[4]),

				new Edge (gvs[5], gvs[1]),
				new Edge (gvs[5], gvs[2]),

				new Edge (gvs[6], gvs[1]),
				new Edge (gvs[6], gvs[3]),

				new Edge (gvs[7], gvs[2]),
				new Edge (gvs[7], gvs[4]),

				new Edge (gvs[8], gvs[3]),
				new Edge (gvs[8], gvs[4])
			};
			g.AddVertexRange (gvs);
			g.AddEdgeRange (ges);

			print (new UllmanSIP (q, g).Result.ToString ()); 

			//var seq = new SEQ (q, g);
			//var lst = seq.GetSEQ ();

			//foreach (var item in lst) {
			//	print (item.ToString ());
			//}
		}

		static void test3() {
			var q = new Graph(false);
			Vertex[] qvs = { 
				new Vertex (0, "A"),
				new Vertex (1, "B"),
				new Vertex (2, "C"),
				new Vertex (3, "A")
			};
			Edge[] qes = {
				new Edge (qvs[0], qvs[1]),
				new Edge (qvs[1], qvs[2]),
				new Edge (qvs[2], qvs[3])
			};
			q.AddVertexRange (qvs);
			q.AddEdgeRange (qes);

			var g = new Graph(false);
			Vertex[] gvs = { 
				new Vertex (0, "A"),
				new Vertex (1, "B"),
				new Vertex (2, "B"),
				new Vertex (3, "C"),
				new Vertex (4, "D"),
				new Vertex (5, "B"),
				new Vertex (6, "A"),
				new Vertex (7, "D"),
				new Vertex (8, "B"),
				new Vertex (9, "C"),
				new Vertex (10, "A"),
				new Vertex (11, "A")
			};
			Edge[] ges = {
				new Edge (gvs[0], gvs[1]),
				new Edge (gvs[0], gvs[2]),
				new Edge (gvs[0], gvs[3]),

				new Edge (gvs[1], gvs[5]),
				new Edge (gvs[2], gvs[4]),
				new Edge (gvs[3], gvs[6]),
				new Edge (gvs[4], gvs[6]),
				new Edge (gvs[5], gvs[6]),
				new Edge (gvs[5], gvs[7]),
				new Edge (gvs[6], gvs[8]),
				new Edge (gvs[7], gvs[8]),

				new Edge (gvs[8], gvs[9]),
				new Edge (gvs[8], gvs[11]),
				new Edge (gvs[9], gvs[10]),
				new Edge (gvs[9], gvs[11])
			};
			g.AddVertexRange (gvs);
			g.AddEdgeRange (ges);
			
			print (new UllmanSIP (q, g).Result.ToString ()); 
			/*
			var seq = new SEQ (q, g);
			var lst = seq.GetSEQ ();

			foreach (var item in lst) {
				print (item.ToString ());
			}*/
		}

		static void test4() {
			var q = new Graph(false);
			Vertex[] qvs = { 
				new Vertex (0, "A"),
				new Vertex (1, "B"),
				new Vertex (2, "B"),
				new Vertex (3, "C")
			};
			Edge[] qes = {
				new Edge (qvs[0], qvs[1]),
				new Edge (qvs[0], qvs[2]),
				new Edge (qvs[1], qvs[2]),
				new Edge (qvs[1], qvs[3])
			};
			q.AddVertexRange (qvs);
			q.AddEdgeRange (qes);

		

			var g = new Graph(false);
			Vertex[] gvs = { 
				new Vertex (0, "A"),
				new Vertex (1, "B"),
				new Vertex (2, "B"),
				new Vertex (3, "A"),
				new Vertex (4, "B"),
				new Vertex (5, "B"),
				new Vertex (6, "A"),
				new Vertex (7, "C"),
				new Vertex (8, "C")
			};
			Edge[] ges = {
				new Edge (gvs[0], gvs[1]),
				new Edge (gvs[0], gvs[2]),

				new Edge (gvs[1], gvs[2]),
				new Edge (gvs[1], gvs[3]),
				new Edge (gvs[1], gvs[4]),
				new Edge (gvs[1], gvs[8]),

				new Edge (gvs[2], gvs[3]),
				new Edge (gvs[2], gvs[5]),
				new Edge (gvs[2], gvs[7]),

				new Edge (gvs[3], gvs[4]),
				new Edge (gvs[3], gvs[5]),

				new Edge (gvs[4], gvs[5]),
				new Edge (gvs[4], gvs[6]),
				new Edge (gvs[4], gvs[8]),

				new Edge (gvs[5], gvs[6]),
				new Edge (gvs[5], gvs[7])
			};
			g.AddVertexRange (gvs);
			g.AddEdgeRange (ges);
			
			print (new UllmanSIP (q, g).Result.ToString ()); 

			/*
			var seq = new SEQ (q, g);
			var lst = seq.GetSEQ ();

			foreach (var item in lst) {
				print (item.ToString ());
			}*/
		}

	}
}

