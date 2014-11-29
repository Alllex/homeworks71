using System;
using System.Collections.Generic;
using QuickGraph;
using System.Linq;

namespace SIP
{
	using Graph = UndirectedGraph<Vertex, Edge>;
	using WeightedGraph = UndirectedGraph<WeightedVertex, WeightedEdge>;
	using Label = String;

	public class SEQ
	{

		List<Item> seq;

		public SEQ(Graph q, Graph g)
		{
			seq = new MinimumSpanningTreeBuilder (q, g).MakeSEQ ();
		}

		public List<Item> GetSEQ()
		{
			return seq;
		}

		public class Item
		{
			public Item(Vertex p, Vertex v, Label l)
			{
				this.Parent = p;
				this.Vertex = v;
				this.Label = l;
			}

			public Vertex Parent { get; private set; }
			public Vertex Vertex { get; private set; }
			public String Label { get; private set; }

			public override string ToString ()
			{
				return string.Format ("[Item: Parent={0}, Vertex={1}, Label={2}]", Parent, Vertex, Label);
			}
		}

		private class Builder
		{
			HashSet<Vertex> vs = new HashSet<Vertex>();
			List<Item> seq = new List<Item>();

			public List<Item> MakeSEQ()
			{
				return seq;
			}

			public void AddEdge(Edge e)
			{
				Vertex inner = null;
				Vertex outer = null;
				if (vs.Contains (e.Source)) {
					inner = e.Source;
					outer = e.Target;
				} else {
					inner = e.Target;
					outer = e.Source;
				}
				seq.Add(new Item(inner, outer, outer.Label));
			}
		}

		private class MinimumSpanningTreeBuilder
		{
			Dictionary<Label, int> vft; // vertex frequency table
			Dictionary<Tuple<Label, Label>, int> eft; // edge frequency table
			WeightedGraph qw;

			public MinimumSpanningTreeBuilder(Graph q, Graph g)
			{
				vft = mkVFT (q, g);
				eft = mkEFT (q, g);
				qw = weightGraph (q);
			}

			public List<Item> MakeSEQ()
			{
				var vCount = (from v in qw.Vertices select v.ID).Distinct ().Count ();
				var vt = new HashSet<int> ();
				var et = new HashSet<WeightedEdge> ();
				var seq = new Builder ();
				var p = lightestEdges (qw);
				var e = selectFirstEdge (p.ToList (), qw);
				et.Add (e);
				vt.Add (e.Source.ID); vt.Add (e.Target.ID);
				seq.AddEdge (e);
				qw.RemoveEdge (e);
				while (vCount > vt.Count()) {
					var tmp_vs = qw.Vertices.ToList ();
					var tmp_es = qw.Edges.ToList ();
					p = front (qw, vt);
					e = selectSpanningEdge (p.ToList (), qw, vt);
					et.Add (e);
					vt.Add ((outer (vt, e)).ID);
					seq.AddEdge (e);
					qw.RemoveEdge (e);
					var inn = from ee in inner (qw, vt) orderby ee.Weight select ee;
					foreach (var ee in inn) {
						seq.AddEdge (ee);
						qw.RemoveEdge (ee);
					}
				}
				return seq.MakeSEQ ();
			}

			Dictionary<Label, int> mkVFT(Graph q, Graph g)
			{
				var	ft = new Dictionary<Label, int> ();
				foreach (var v in q.Vertices) {
					if (!ft.ContainsKey (v.Label)) {
						ft [v.Label] = g.Vertices.Count (v2 => v2.Label.Equals (v.Label));
					}
				}
				return ft;
			}
			
			Dictionary<Tuple<Label, Label>, int> mkEFT(Graph q, Graph g)
			{
				var ft = new Dictionary<Tuple<Label, Label>, int> ();
				foreach (var e in q.Edges) {
					var key = Tuple.Create (e.Source.Label, e.Target.Label);
					var key2 = Tuple.Create (e.Target.Label, e.Source.Label);
					if (!(ft.ContainsKey (key) || ft.ContainsKey (key2))) {
						ft [key] = g.Edges.Count (e2 => Edge.UndirectedEquals (e, e2));
						ft [key2] = ft [key];
					}
				}
				return ft;
			}

			WeightedGraph weightGraph(Graph q)
			{
				var qq = new WeightedGraph (false);
				qq.AddVerticesAndEdgeRange (
					from e in q.Edges select weightEdge(e)
				);
				return qq;
			}

			WeightedEdge weightEdge(IEdge<Vertex> e)
			{
				var src = new WeightedVertex (e.Source.ID, e.Source.Label, vft [e.Source.Label]);
				var tgt = new WeightedVertex (e.Target.ID, e.Target.Label, vft [e.Target.Label]);
				var w = eft [Tuple.Create (src.Label, tgt.Label)];
				return new WeightedEdge(src, tgt, w);
			}

			static IEnumerable<WeightedEdge> lightestEdges(WeightedGraph q)
			{
				var minWeight = q.Edges.Min(e => e.Weight);
				return q.Edges.Where (e => e.Weight == minWeight);
			}

			static WeightedEdge selectFirstEdge(IEnumerable<WeightedEdge> p, WeightedGraph q)
			{
				if (p.Count() > 1) {
					Func<WeightedVertex, int> degree = v => q.AdjacentEdges(v).Count();
					Func<WeightedEdge, int> edgeDegree = e => degree (e.Source) + degree (e.Target);
					var minEdgeDegree = q.Edges.Min (edgeDegree);
					p = p.Where (e => edgeDegree (e) == minEdgeDegree);
				}
				return p.First ();
			}

			static int indGCount(WeightedGraph q, HashSet<int> vs)
			{
				return q.Edges.Count ( e =>
				    vs.Contains (e.Source.ID) && vs.Contains (e.Target.ID)
				);
			}

			static WeightedVertex outer(HashSet<int> vs, WeightedEdge e)
			{
				return (vs.Contains (e.Source.ID)) ? e.Target : e.Source;
			}

			static WeightedEdge selectSpanningEdge(IEnumerable<WeightedEdge> p, WeightedGraph q, HashSet<int> vs)
			{
				if (p.Count () > 1) {
					Func<WeightedEdge, int> ind = e => {
						var o = outer (vs, e);
						vs.Add (o.ID);
						var indg = indGCount (q, vs);
						vs.Remove (o.ID);
						return indg;
					};
					var maxIndG = q.Edges.Max (ind);
					p = p.Where (e => ind (e) == maxIndG);
				}
				if (p.Count () > 1) {
					Func<WeightedEdge, int> outerDegree = e => q.AdjacentEdges(outer (vs, e)).Count();
					var minDegree = q.Edges.Min (outerDegree);
					p = p.Where (e => outerDegree (e) == minDegree);
				}
				return p.First ();
			}
		
			static IEnumerable<WeightedEdge> front(WeightedGraph q, HashSet<int> vs)
			{
				List<WeightedEdge> es = new List<WeightedEdge> ();
				foreach (var e in q.Edges) {
					var src = e.Source;
					var tgt = e.Target;
					var srcin = vs.Contains (src.ID);
					var tgtin = vs.Contains (tgt.ID);
					if (srcin ^ tgtin) {
						es.Add (e);
					}
				}
				return es;
			}

			static IEnumerable<WeightedEdge> inner(WeightedGraph q, HashSet<int> vs)
			{
				return from e in q.Edges where vs.Contains (e.Source.ID) && vs.Contains (e.Target.ID) select e;
			}

		}

	}


}

