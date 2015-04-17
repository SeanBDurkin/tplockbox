The main features of TPLB3 shall be:

The user interface shall be clean and simple. For Ciphers and Hashes, two styles shall be provided: A component and an interface pointer.
IV, salting of ciphers and signalling of IV's shall be managed and hidden from the developer-client.
The main encryption functions shall be implemented in 100% native Delphi code. (TOpenSSL\_Signatory component is the exception to the rule).

That is to say it shall not rely on links to third party libraries (at least as far as the core functionality is concerned).

Supports Delphi/Pascal version:
Delphi XE2; (platforms: Win 32-bit and Win 64-bit).
Delphi XE;
Delphi 2010;
Delphi 2009;
Delphi 2007;
Delphi 2005;
Delphi 7;
Easy traceability to standards. Developers should be able to open cipher standards and open the respective implementing source code; put them side-by-side, and very quickly observer that one implements the other. The implementing source should borrow the style and symbols and the specifiying cipher standard.
It shall be as easy as it can be to extend the library with new hashes and block ciphers.
The selection, implementation and usage of ciphers shall be divorced from the chaining mode.