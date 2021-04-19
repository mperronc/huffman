/**
 * Matthias PERRONCEL
 * Tudwall CREZE
 * Groupe MA1-A
 */

package fr.istic.si2.huffman

/**
 * Type algébrique simple modélisant les bits (0 ou 1)
 */
sealed trait Bit
case object Zero extends Bit
case object One extends Bit

/**
 * Type algébrique récursif modélisant les arbres de code de Huffman
 */
sealed trait Huffman
case class Feuille(freq: Double, c: Char) extends Huffman
case class Noeud(freq: Double, zero: Huffman, one: Huffman) extends Huffman

/**
 * Type algébrique simple utilisé dans l'app V3 pour représenter le mode
 * d'utilisation
 */
sealed trait Mode
case object ModeCodage extends Mode
case object ModeDecodage extends Mode

