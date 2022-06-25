# DistinguishedSymmetricCycles.hs

A Haskell module DistinguishedSymmetricCycles.hs exporting a couple of
functions that describe a `(linear algebraic)-computation-free' decomposition
of a vertex of a hypercube graph with respect to its distinguished symmetric cycle. 
Based on Proposition 4.9 of the monograph A.O. Matveev, Symmetric Cycles: 
A 2D Perspective on Higher Dimensional Discrete Hypercubes, the Power Sets
of Finite Sets, and Set Families. Leanpub, 2022, https://leanpub.com/SymmetricCycles.  

# Hypercube Graphs

Let $t$ be a positive integer, $t\geq 3$. We call the set of integers~$E_t:=[t]:=\{1,\ldots,t\}$ the~{\em ground set}.

The {\em vertex set\/} of the {\em hypercube graph\/}~$\boldsymbol{H}(t,2)$ by convention is the set $\{1,-1\}^t$, that is, the $t$-dimensional {\em discrete hypercube}. We regard the discrete hypercube~$\{1,-1\}^t$ as a $2^t$-subset of elements of the real Euclidean space~$\mathbb{R}^t$ of {\em row\/} vectors~$T:=(T(1),\ldots,T(t))$.

The vertex~$\mathrm{T}^{(+)}:=(1,\ldots,1)$ whose components are all~$1$'s is called the {\em positive vertex}.

The {\em negative part\/} $\boldsymbol{\mathfrak{n}}(T)$ of a vertex $T\in\{1,-1\}^t$ is defined by
\begin{equation*}
\boldsymbol{\mathfrak{n}}(T):=\{e\in E_t\colon T(e)=-1\}\; .
\end{equation*}

An unordered pair~$\{T',T''\}\subset\{1,-1\}^t$ by convention is an {\em edge\/} of the hypercube graph~$\boldsymbol{H}(t,2)$ if and only if
we have
\begin{equation*}
|\{e\in E_t\colon T'(e)\neq T''(e)\}|=1\; ,
\end{equation*}
that is, the {\em Hamming distance\/} between the vertices~$T'$ and~$T''$ equals $1$.

We define the so-called {\em distinguished symmetric cycle\/} $\boldsymbol{R}:=(R^0,R^1,\ldots,$ $R^{2t-1},R^0)$ in the graph~$\boldsymbol{H}(t,2)$ by
\begin{align*}
R^0 :\!&= \mathrm{T}^{(+)}\; ,\\
R^s :\!&= {}_{-[s]}R^0\; ,\ \ \ 1\leq s\leq t-1\; ,\\
\intertext{and}
R^{t+k} :\!&= -R^k\; ,\ \ \ 0\leq k\leq t-1\; ,
\end{align*}
where
\begin{equation*}
{}_{-[s]}R^0 := {}_{-[s]}\mathrm{T}^{(+)} := (\underbrace{-1,\ldots,-1}_{s},\underbrace{1,\ldots,1}_{t-s})\; .
\end{equation*}

Since the square matrix
\begin{equation*}
\mathbf{M}:=\mathbf{M}(\boldsymbol{R}):=\left(
\begin{smallmatrix}
R^0\\ R^1\vspace{-2mm}\\ \vdots\\ R^{t-1}
\end{smallmatrix}
\right)\in\mathbb{R}^{t\times t}\; ,
\end{equation*}
whose rows and columns are indexed starting with $1$, is {\em nonsingular}, the sequence~$(R^0,R^1,\ldots,R^{t-1})$ is an ordered {\em basis\/}
of the space~$\mathbb{R}^t$.

Given an arbitrary vertex~$T\in\{1,-1\}^t$ of the graph~$\boldsymbol{H}(t,2)$, we thus have
\begin{equation*}
T=\boldsymbol{x}\cdot\mathbf{M}\; ,
\end{equation*}
for the {\em unique\/} row `$\boldsymbol{x}$-vector'
\begin{equation*}
\boldsymbol{x}:=\boldsymbol{x}(T,\boldsymbol{R}):=(x_1,\ldots,x_t):= T\cdot\mathbf{M}^{-1}\; .
\end{equation*}
Recall that we have
\begin{equation*}
\boldsymbol{x}\in\{-1,0,1\}^t\; . 
\end{equation*}
The {\em decomposition set\/}
\begin{equation*}
\boldsymbol{Q}(T,\boldsymbol{R}):=\{x_i\cdot R^{i-1}\colon x_i\neq 0\}
\end{equation*}
{\em for the vertex\/}~$T$ {\em with respect to the cycle\/}~$\boldsymbol{R}$ is the {\em unique inclusion-minimal\/} subset of the vertex set of the cycle~$\boldsymbol{R}$ such that
\begin{equation*}
T=\sum_{Q\in\boldsymbol{Q}(T,\boldsymbol{R})}Q\; .
\end{equation*}
The subset~$\boldsymbol{Q}(T,\boldsymbol{R})\subset\mathbb{R}^t$, of {\em odd\/} cardinality, is {\em linearly independent}.

# Distinguished Symmetric Cycles in Hypercube Graphs and Computation-free Vertex Decompositions

In order to avoid the above basic {\em linear algebraic\/} technique, we use a `computation-free' approach to finding the 
vectors $\boldsymbol{x}({}_{-A}\mathrm{T}^{(+)},\boldsymbol{R})$ and the decomposition sets~$\boldsymbol{Q}({}_{-A}\mathrm{T}^{},\boldsymbol{R})$, as explained in 
an accompanying PDF-note.
