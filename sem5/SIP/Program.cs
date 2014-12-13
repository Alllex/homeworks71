using System;
using QuickGraph;
using System.Diagnostics;

namespace SIP
{
	using Graph = UndirectedGraph<Vertex, Edge>;
	using WeightedGraph = UndirectedGraph<WeightedVertex, WeightedEdge>;

	class MainClass
	{

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

			print (new QuickSI (q, g).Result.ToString ()); 

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
			
			print (new QuickSI (q, g).Result.ToString ()); 
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
			
			print (new QuickSI (q, g).Result.ToString ()); 

			/*
			var seq = new SEQ (q, g);
			var lst = seq.GetSEQ ();

			foreach (var item in lst) {
				print (item.ToString ());
			}*/
		}

		public static void Main (string[] args)
		{
			Console.WriteLine ("Hello World!");

			test2 ();
			test3 ();
			test4 ();

			using (System.IO.StreamWriter file = new System.IO.StreamWriter(@"time.txt")) file.WriteLine("");
			using (System.IO.StreamWriter file = new System.IO.StreamWriter(@"vcount.txt")) file.WriteLine("");

			for (int vCount = 2; vCount < 31; vCount += 2) {
				var count = 10;
				var succ = 0;
				var watch = Stopwatch.StartNew();
				// the code that you want to measure comes here
				for (int i = 0; i < count; i++) {
					var q = RandomGraph.GenerateConnected (vCount, 75);
					var g = RandomGraph.ExtendConnected (q, 4 * vCount, 50);
					//printGraph (q);
					//printGraph (g); 

					var result = new QuickSI (q, g).Result; 
					if (result)
						succ++;
				}

				watch.Stop();
				var secs = (float) watch.ElapsedMilliseconds / 1000;

				using (System.IO.StreamWriter file = new System.IO.StreamWriter(@"time.txt", true))
				{
					file.WriteLine(secs);
				}

				using (System.IO.StreamWriter file = new System.IO.StreamWriter(@"vcount.txt", true))
				{
					file.WriteLine(vCount);
				}

				//print (secs.ToString ()); 
				print (vCount + " -> " + succ + "/" + count + "  --  " + secs + "s"); 
			}

		}
	}
}

