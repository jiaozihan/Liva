; ModuleID = 'Liva'

%test = type <{ i32 }>

@tmp = private unnamed_addr constant [13 x i8] c"Hello World!\00"
@tmp.1 = private unnamed_addr constant [3 x i8] c"%s\00"

declare i32 @printf(i8*, ...)

declare i8* @malloc(i32)

define i64* @lookup(i32 %c_index, i32 %f_index) {
entry:
  %tmp = alloca i64**
  %tmp1 = alloca i64*, i32 0
  %tmp2 = getelementptr i64**, i64*** %tmp, i32 0
  store i64** %tmp1, i64*** %tmp2
  ret i64* null
}

define %test* @test.constructor() {
entry:
  %this = alloca %test
  %tmp = call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i32))
  %tmp1 = bitcast i8* %tmp to %test*
  %tmp2 = load %test, %test* %tmp1
  store %test %tmp2, %test* %this
  %.key = getelementptr inbounds %test, %test* %this, i32 0, i32 0
  store i32 0, i32* %.key
  ret %test* %this
}

define i32 @main() {
entry:
  %tmp = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @tmp.1, i32 0, i32 0), i8* getelementptr inbounds ([13 x i8], [13 x i8]* @tmp, i32 0, i32 0))
  ret i32 0
}
