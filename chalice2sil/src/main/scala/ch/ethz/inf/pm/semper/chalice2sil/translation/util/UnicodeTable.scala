package ch.ethz.inf.pm.semper.chalice2sil.translation.util

import collection.immutable
import ch.ethz.inf.pm.semper.chalice2sil.util.Trie
import immutable.WrappedString

/**
  * @author Christian Klauser
  */
object UnicodeTable {
  val backward = immutable.Map[String, Char](
    "alpha" -> 'α',
    "beta" -> 'β',
    "gamma" -> 'γ',
    "delta" -> 'δ',
    "epsilon" -> 'ϵ',
    "varepsilon" -> 'ε',
    "zeta" -> 'ζ',
    "eta" -> 'η',
    "theta" -> 'θ',
    "iota" -> 'ι',
    "kappa" -> 'κ',
    "lambda" -> 'λ',
    "mu" -> 'μ',
    "nu" -> 'ν',
    "xi" -> 'ξ',
    "omicron" -> 'ο',
    "pi" -> 'π',
    "rho" -> 'ρ',
    "sigma" -> 'σ',
    "varsigma" -> 'ς',
    "tau" -> 'τ',
    "upsilon" -> 'υ',
    "phi" -> 'ϕ',
    "varphi" -> 'φ',
    "chi" -> 'χ',
    "psi" -> 'ψ',
    "omega" -> 'ω',

    "Alpha" -> 'Α',
    "Beta" -> 'Β',
    "Gamma" -> 'Γ',
    "Delta" -> 'Δ',
    "Epsilon" -> 'Ε',
    "Zeta" -> 'Ζ',
    "Eta" -> 'Η',
    "Theta" -> 'Θ',
    "Iota" -> 'Ι',
    "Kappa" -> 'Κ',
    "Lambda" -> 'Λ',
    "Mu" -> 'Μ',
    "Nu" -> 'Ν',
    "Xi" -> 'Ξ',
    "Omicron" -> 'Ο',
    "Pi" -> 'Π',
    "Rho" -> 'Ρ',
    "Sigma" -> 'Σ',
    "Tau" -> 'Τ',
    "Upsilon" -> 'Υ',
    "Phi" -> 'Φ',
    "Chi" -> 'Χ',
    "Psi" -> 'Ψ',
    "Omega" -> 'Ω',

    "infty" -> '∞',
    "leq" -> '≤',
    "geq" -> '≥',
    "ne" -> '≠',
    "elem" -> '∈',
    "ni" -> '∋',
    "owns" -> '∋',
    "approx" -> '≈',
    "equiv" -> '≡',
    "subset" -> '⊂',
    "eqsubset" -> '⊆',
    "superset" -> '⊃',
    "eqsuperset" -> '⊇',
    "cup" -> '∪',
    "cap" -> '∩',
    "land" -> '∧',
    "wedge" -> '∧',
    "lor" -> '∨',
    "vee" -> '∨',
    "times" -> '×',
    "forall" -> '∀',
    "exists" -> '∃',
    "bot" -> '⊥',
    "to" -> '→',
    "imply" -> '→',
    "entail" -> '⊢',
    "leftrightarrow" -> '↔')
  val forward : immutable.Map[Char,String] = backward.map(t => t._2 -> t._1)

  lazy val backwardTrie = {
    val trie = Trie.create[WrappedString, Char, Char](backward.map(t => ((t._1 : WrappedString),t._2)))
    assert(!trie.allowsShortestMatch.isDefined,{
      val t = trie.allowsShortestMatch.get
      "Cannot form Trie that supports unambiguous shortest match. Have a look at entry for %s around character %s.".format(t.value.get, t.element)
    })
    trie
  }
}
