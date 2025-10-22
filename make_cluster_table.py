#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Create a LaTeX table of meta-groups (Semantic groups) and their clusters/medoids
from the provided JSON structure.

Usage:
  python make_cluster_table.py input.json [-o clusters_table.tex]

Notes:
- Output is a LaTeX longtable using booktabs. It’s UTF-8; compile with XeLaTeX/LuaLaTeX.
- Columns: Semantic group | Cluster ID | Medoid | Size | Cluster label
- If you only want some columns, remove them in the header/rows below.
"""

import json
import argparse
import sys
import re
from pathlib import Path

# Minimal LaTeX escaper (keeps UTF-8 Greek; only escapes specials)
_LATEX_SPECIALS = {
    '\\': r'\textbackslash{}',
    '&': r'\&',
    '%': r'\%',
    '$': r'\$',
    '#': r'\#',
    '_': r'\_',
    '{': r'\{',
    '}': r'\}',
    '~': r'\textasciitilde{}',
    '^': r'\textasciicircum{}',
}

def tex_escape(s: str) -> str:
    if s is None:
        return ''
    # First ensure it's a string
    s = str(s)
    # Replace backslash first to avoid double-escaping
    s = s.replace('\\', _LATEX_SPECIALS['\\'])
    # Then the rest
    return re.sub(r'([&%$#_\{\}~^])', lambda m: _LATEX_SPECIALS[m.group(1)], s)

def to_int_or_str(x):
    """Try to convert cluster ids like '2' to int for sorting; fallback to str."""
    try:
        return int(x)
    except Exception:
        return str(x)

def build_rows(data):
    rows = []
    meta_groups = data.get("meta_groups", [])
    # Sort meta-groups by id if present, else by label
    meta_groups_sorted = sorted(
        meta_groups,
        key=lambda mg: (mg.get("meta_group_id", 10**9), mg.get("meta_label", ""))
    )
    for mg in meta_groups_sorted:
        meta_label = mg.get("meta_label") or ""
        clusters = mg.get("clusters", [])
        # Sort clusters by numeric cluster_id if possible
        clusters_sorted = sorted(clusters, key=lambda c: to_int_or_str(c.get("cluster_id", "")))
        for c in clusters_sorted:
            rows.append({
                "meta_label": meta_label,
                "cluster_id": c.get("cluster_id", ""),
                "medoid": c.get("medoid", ""),
                "size": c.get("size", ""),
                "label": c.get("label", "")
            })
    return rows

def render_longtable(rows):
    header = r"""\begin{longtable}{@{}p{0.28\textwidth} p{0.09\textwidth} p{0.25\textwidth} p{0.08\textwidth} p{0.26\textwidth}@{}}
\caption{Semantic groups (meta-groups) with clusters and their medoids}\label{tab:meta_groups_clusters}\\
\toprule
\textbf{Semantic group} & \textbf{Cluster} & \textbf{Medoid} & \textbf{Size} & \textbf{Cluster label} \\
\midrule
\endfirsthead
\toprule
\textbf{Semantic group} & \textbf{Cluster} & \textbf{Medoid} & \textbf{Size} & \textbf{Cluster label} \\
\midrule
\endhead
\midrule
\multicolumn{5}{r}{\emph{Continued on next page}}\\
\bottomrule
\endfoot
\bottomrule
\endlastfoot
"""
    lines = [header]
    for r in rows:
        lines.append(
            f"{tex_escape(r['meta_label'])} & "
            f"{tex_escape(r['cluster_id'])} & "
            f"{tex_escape(r['medoid'])} & "
            f"{tex_escape(r['size'])} & "
            f"{tex_escape(r['label'])} \\\\"
        )
    lines.append(r"\end{longtable}")
    return "\n".join(lines)

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("input_json", help="Path to input JSON file")
    ap.add_argument("-o", "--output", help="Path to output .tex (default: stdout)")
    args = ap.parse_args()

    in_path = Path(args.input_json)
    if not in_path.exists():
        print(f"Error: {in_path} not found", file=sys.stderr)
        sys.exit(1)

    with in_path.open("r", encoding="utf-8") as f:
        data = json.load(f)

    rows = build_rows(data)
    tex = render_longtable(rows)

    if args.output:
        out_path = Path(args.output)
        out_path.write_text(tex, encoding="utf-8")
    else:
        sys.stdout.write(tex)

if __name__ == "__main__":
    main()

