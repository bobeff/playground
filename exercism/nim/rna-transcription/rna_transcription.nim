import std/[tables, enumerate]

const
  complements = {
    'G': 'C',
    'C': 'G',
    'T': 'A',
    'A': 'U'}.toTable

proc toRna*(dna: string): string =
  result = newString(dna.len)
  for i, nucleotide in enumerate(dna): 
    if nucleotide notin {'G', 'C', 'T', 'A'}:
      raise newException(ValueError, "Invalid nucleotide: " & nucleotide)
    result[i] = complements[nucleotide]
