structure Bzip2 :> BZIP2 = struct
	structure Primitive :> sig
		eqtype bz2file
		val null: bz2file
		val bz2open: string * string -> bz2file
		val bz2read: bz2file * CharArray.array * word * word -> int
		val bz2writea: bz2file * CharArray.array * word * word -> int
		val bz2writev: bz2file * CharVector.vector * word * word -> int
		val bz2close: bz2file -> int
	end = struct
		type bz2file = MLton.Pointer.t
		val null = MLton.Pointer.null
		val bz2open = _import "BZ2_bzopen": string * string -> bz2file;
		val bz2read = _import "bz2readoffset":
			bz2file * CharArray.array * word * word -> int;
		val bz2writea = _import "bz2writeoffset":
			bz2file * CharArray.array * word * word -> int;
		val bz2writev = _import "bz2writeoffset":
			bz2file * CharVector.vector * word * word -> int;
		val bz2close = _import "BZ2_bzclose": bz2file -> int;
	end

	exception Failure
	fun readArraySlice (g, slice) =
		let
			val (array, offset, length) = CharArraySlice.base slice
			val wordoffset = Word.fromInt offset
			val wordlength = Word.fromInt length
		in
			Primitive.bz2read (g, array, wordoffset, wordlength)
		end
	fun readerFromPrimitive (name, g) =
		let
			val closed = ref false
			fun error x = raise IO.Io {
				name = name
				, function = "readArr"
				, cause = x
			}
			fun readArr slice =
				if !closed then error IO.ClosedStream
				else let
					val r = readArraySlice (g, slice)
				in
					if r < 0 then error Failure
					else r
				end
			fun close () =
				if !closed then ()
				else (
					closed := true
					; ignore (Primitive.bz2close g)
				)
		in
			TextPrimIO.augmentReader (TextPrimIO.RD {
				name = name
				, chunkSize = 32 * 1024
				, readVec = NONE
				, readArr = SOME readArr
				, readVecNB = NONE
				, readArrNB = NONE
				, block = NONE
				, canInput = NONE
				, avail = fn () => NONE
				, getPos = NONE
				, setPos = NONE
				, endPos = NONE
				, verifyPos = NONE
				, close = close
				, ioDesc = NONE
			})
		end
	fun reader name =
		let
			val path = name ^ "\000"
			val g = Primitive.bz2open (path, "r\000")
		in
			if g = Primitive.null then NONE
			else SOME (readerFromPrimitive (name, g))
		end
	fun openIn name =
		case reader name of
			SOME x => TextIO.mkInstream (TextIO.StreamIO.mkInstream (x, ""))
			| NONE => raise IO.Io {
				name = name
				, function = "openIn"
				, cause = Failure
			}
	fun writeSlice (base, write) (g, slice) =
		let
			val (vector, offset, length) = base slice
			val wordoffset = Word.fromInt offset
			val wordlength = Word.fromInt length
		in
			write (g, vector, wordoffset, wordlength)
		end
	val writeVectorSlice = writeSlice (CharVectorSlice.base, Primitive.bz2writev)
	val writeArraySlice = writeSlice (CharArraySlice.base, Primitive.bz2writea)
	fun writerFromPrimitive (name, g) =
		let
			val closed = ref false
			fun error (function, x) = raise IO.Io {
				name = name
				, function = function
				, cause = x
			}
			fun close () =
				if !closed then ()
				else (
					closed := true
					; ignore (Primitive.bz2close g)
				)
			fun write (name, realWrite) slice =
				if !closed then error (name, IO.ClosedStream)
				else let
					val r = realWrite (g, slice)
				in
					if r <= 0 then error (name, Failure)
					else r
				end
			val writeVec = write ("writeVec", writeVectorSlice)
			val writeArr = write ("writeArr", writeArraySlice)
		in
			TextPrimIO.augmentWriter (TextPrimIO.WR {
				name = name
				, chunkSize = 32 * 1024
				, writeVec = SOME writeVec
				, writeArr = SOME writeArr
				, writeVecNB = NONE
				, writeArrNB = NONE
				, block = NONE
				, canOutput = NONE
				, getPos = NONE
				, setPos = NONE
				, endPos = NONE
				, verifyPos = NONE
				, close = close
				, ioDesc = NONE
			})
		end
	fun writer name =
		let
			val path = name ^ "\000"
			val g = Primitive.bz2open (path, "w9")
		in
			if g = Primitive.null then NONE
			else SOME (writerFromPrimitive (name, g))
		end
	fun openOut name =
		case writer name of
			SOME x => TextIO.mkOutstream (TextIO.StreamIO.mkOutstream (x, IO.NO_BUF))
			| NONE => raise IO.Io {
				name = name
				, function = "openOut"
				, cause = Failure
			}
end

