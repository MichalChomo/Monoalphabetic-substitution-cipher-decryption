# Subs-cipher
This is a project for Functional and Logic Programming course. It is an application written in Haskell programming language for deciphering monoalphabetic substitution cipher by ciphertext-only attack. It works as follows:
1. Frequency analysis on letters in ciphertext is performed.
2. If some letters are missing in ciphertext, they are added with zero frequency
3. Mapping of ciphertext letters to plaintext letters is created according to frequency database -> letter key
4. Frequency analysis on digrams in ciphertext is performed.
5. Mapping of ciphertext digrams to plaintext digrams is created according to frequency database, this is then converted to mapping of letter to letter -> digram key
6. Letter key and digram key are combined, when ciphertext letters match, digram key has priority, so its plaintext is used
7. Frequency analysis on trigrams in ciphertext is performed.
8. Mapping of ciphertext trigrams to plaintext trigrams is created according to frequency database, this is then converted to mapping of letter to letter -> trigram key
9. Existing key (combined letter and digram keys) and trigram key are combined, when ciphertext letters match, trigram key has priority, so its plaintext is used -> final key
10. Final key is modified so it does not contain two ciphertext letters mapping to the same plaintext letter, if some ciphertext letters do not have a plaintext letter mapping, '-' is used.

*Usage:*
./subs-cipher [-k] [-t] frequency_database [ciphertext]
* -k                  output key                                                  
* -t                  output deciphered text                                      
* frequency_database  file containing frequency database                          
* [ciphertext]        file containing frequency database, stdin is used if missing
