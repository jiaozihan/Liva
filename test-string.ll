; ModuleID = 'Liva'

%test = type <{ i32 }>
%String = type <{ i32, i32, i8* }>

@tmp = private unnamed_addr constant [3 x i8] c"z=\00"
@tmp1 = private unnamed_addr constant [3 x i8] c"%s\00"

declare i32 @printf(i8*, ...)

declare i8* @malloc(i32)

define i64* @lookup(i32 %c_index, i32 %f_index) {
entry:
  %tmp = alloca i64**, i32 2
  %tmp1 = alloca i64*, i32 0
  %tmp2 = getelementptr i64*** %tmp, i32 1
  store i64** %tmp1, i64*** %tmp2
  %tmp3 = alloca i64*
  %tmp4 = getelementptr i64** %tmp3, i32 0
  store i64* bitcast (i32 (%String*)* @String.length to i64*), i64** %tmp4
  %tmp5 = getelementptr i64*** %tmp, i32 0
  store i64** %tmp3, i64*** %tmp5
  %tmp6 = getelementptr i64*** %tmp, i32 %c_index
  %tmp7 = load i64*** %tmp6
  %tmp8 = getelementptr i64** %tmp7, i32 %f_index
  %tmp9 = load i64** %tmp8
  ret i64* %tmp9
}

define %test* @test.constructor() {
entry:
  %this = alloca %test
  %tmp = call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1** null, i32 1) to i32))
  %tmp1 = bitcast i8* %tmp to %test*
  %tmp2 = load %test* %tmp1
  store %test %tmp2, %test* %this
  %.key = getelementptr inbounds %test* %this, i32 0, i32 0
  store i32 1, i32* %.key
  ret %test* %this
}

define i32 @String.length(%String* %this) {
entry:
  ret i32 1
}

define %String* @String.constructor() {
entry:
  %this = alloca %String
  %tmp = call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1** null, i32 1) to i32))
  %tmp1 = bitcast i8* %tmp to %String*
  %tmp2 = load %String* %tmp1
  store %String %tmp2, %String* %this
  %.key = getelementptr inbounds %String* %this, i32 0, i32 0
  store i32 0, i32* %.key
  ret %String* %this
}

define i32 @main() {
entry:
  %this = alloca %test
  %tmp = call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1** null, i32 1) to i32))
  %tmp1 = bitcast i8* %tmp to %test*
  %tmp2 = load %test* %tmp1
  store %test %tmp2, %test* %this
  %.key = getelementptr inbounds %test* %this, i32 0, i32 0
  store i32 1, i32* %.key
  %tmp3 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([3 x i8]* @tmp1, i32 0, i32 0), i8* getelementptr inbounds ([3 x i8]* @tmp, i32 0, i32 0))
  ret i32 0
}
