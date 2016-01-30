Implementation of the Typed Call-by-Value λ-calculus using a Stack of Regions
---

> My implementation and related notes

## Problems about paper

1. Where will `at` appear and why?
2. What does "size of region" means?
3. Why is "effect" encoded in the type scheme?
4. Is their any special restriction over the recursive `letrec`?
5. The type et cetera is just a temporary thing? Or should I pass them down in the implementation?

## Idea
First, we will implement the algorithms introduced in the paper. Then the *target language* will go through a second pass into a simplified subset of Rust with same-level annotation. Then, the simplified version will be transformed into real Rust code.


## Algorithmic Translation
After some experimenting and struggling, it seems to me that a algorithm W version of translation rules are here to come.

In $W(TE, e) = (S, e', \tau, \epsilon)$,

* $TE: x \mapsto (\sigma, p), f \mapsto (\pi, p)$
* $e$: Source expression
* $e'$: Target expression
* $\tau$: Concrete type
* $\epsilon$: Effects
* $S$: Substitution, written as $[\tau_i / \alpha_i][p_i / \rho_i][\epsilon_i / \phi_i]$

Rules of $TE \Vdash e \Rightarrow e' : \mu, \phi, S$:


$$\frac{TE(x) = (\sigma, p) \quad \tau, S = inst(\sigma)}
        {TE \Vdash x \Rightarrow x : (\tau, p), \emptyset, S}\text{T-VAR}$$

$$\frac{TE(f) = (\pi, p') \quad \pi = \forall \rho_i \dots \quad \tau, S = inst(\pi) \quad
        p \leftarrow newrv, \phi = \{ get(p'), put(p) \}}
        {TE \Vdash f \Rightarrow  f[S(\rho_i)] \texttt{ at } p : (\tau, p), \phi, S} \text{T-FUN}$$

$$\frac{TE \Vdash e_1 \Rightarrow e_1' : \mu_1, \phi_1, S_1 \quad
        S_1(TE) \Vdash e_2 \Rightarrow e_2' : \mu_2, \phi_2, S_2 \quad
        V = U(\mu_1, (\mu_2 \rightarrow^{\epsilon.\phi_0} \mu, p))}
        {TE \Vdash e_1 \, e_2 \Rightarrow e_1' \, e_2' : V(\mu), V(\phi_0) \cup \phi_1 \cup \phi_2 \cup \{ V(\epsilon), get(V(p)) \}, VS_2S_2}\text{T-APP}$$

$$\frac{TE, x: \mu_1 \Vdash e \Rightarrow e': \mu_2, \phi, S}
        {TE \Vdash \lambda x.e \Rightarrow \lambda x . e' \texttt{ at }
        p : (S(\mu_1) \rightarrow^{\epsilon . \phi} \mu_2, p), \{ put(p) \}, S }\text{T-ABS}$$

$$\frac{TE \Vdash e_1 \Rightarrow e_1': (\tau_1, p_1), \phi_1, S_1 \quad
        S_1(TE), x: (\overline{S_1 TE}(\tau_1), p_1) \Vdash
        e_2 \Rightarrow e_2' : \mu, \phi_2, S_2}
        {TE \Vdash \texttt{let }x = e_1 \texttt{ in } e_2 \texttt{ end}
        \Rightarrow \texttt{let }x = e_1' \texttt{ in } e_2' \texttt{ end } : \mu,
        \phi_1 \cup \phi_2, S_2S_1}\text{T-LET}$$

$$\frac{TE, f : (\tau, p) \Vdash \lambda x . e_1 \Rightarrow \lambda x. e_1'
        \texttt{ at } p : (\tau', p), \phi_1, S_1 \quad
        \vec \rho = frv(S_1(\tau)) \quad
        S_1(TE), f: (\overline{S_1 TE}(\tau'), p) \Vdash e_2 \Rightarrow e_2' : \mu, \phi_2, S_2}
        {TE \Vdash \texttt{letrec } f(x)= e_1 \texttt{ in } e_2 \texttt{ end } \Rightarrow
         \texttt{letrec } f[\vec \rho](x) \texttt{ at } p = e_1' \texttt{ in } e_2' \texttt{ end } : \mu, \phi_1 \cup \phi_2, S_2 S_1}\text{T-LETREC}$$

