; ModuleID = 'Liva'

%test = type <{ i32 }>
%myclass = type <{ i32, i32 }>
%subclass = type <{ i32, i32 }>

@tmp = private unnamed_addr constant [3 x i8] c"z=\00"
@tmp1 = private unnamed_addr constant [5 x i8] c"%s%d\00"

declare i32 @printf(i8*, ...)

declare i8* @malloc(i32)

define i64* @lookup(i32 %c_index, i32 %f_index) {
entry:
  %tmp = alloca i64**, i32 3
  %tmp1 = alloca i64*, i32 0
  %tmp2 = getelementptr i64*** %tmp, i32 2
  store i64** %tmp1, i64*** %tmp2
  %tmp3 = alloca i64*
  %tmp4 = getelementptr i64** %tmp3, i32 0
  store i64* bitcast (i32 (%myclass*, i32, i32)* @subclass.calc to i64*), i64** %tmp4
  %tmp5 = getelementptr i64*** %tmp, i32 1
  store i64** %tmp3, i64*** %tmp5
  %tmp6 = alloca i64*
  %tmp7 = getelementptr i64** %tmp6, i32 0
  store i64* bitcast (i32 (%myclass*, i32, i32)* @myclass.calc to i64*), i64** %tmp7
  %tmp8 = getelementptr i64*** %tmp, i32 0
  store i64** %tmp6, i64*** %tmp8
  %tmp9 = getelementptr i64*** %tmp, i32 %c_index
  %tmp10 = load i64*** %tmp9
  %tmp11 = getelementptr i64** %tmp10, i32 %f_index
  %tmp12 = load i64** %tmp11
  ret i64* %tmp12
}

define %test* @test.constructor() {
entry:
  %this = alloca %test
  %tmp = call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1** null, i32 1) to i32))
  %tmp1 = bitcast i8* %tmp to %test*
  %tmp2 = load %test* %tmp1
  store %test %tmp2, %test* %this
  %.key = getelementptr inbounds %test* %this, i32 0, i32 0
  store i32 2, i32* %.key
  ret %test* %this
}

define i32 @subclass.calc(%myclass* %this, i32 %x, i32 %y) {
entry:
  %casted = bitcast %myclass* %this to %subclass*
  %z = alloca i32
  %subtmp = sub i32 %x, %y
  store i32 %subtmp, i32* %z
  %z1 = load i32* %z
  ret i32 %z1
}

define %subclass* @subclass.constructor() {
entry:
  %this = alloca %subclass
  %tmp = call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1** null, i32 1) to i32))
  %tmp1 = bitcast i8* %tmp to %subclass*
  %tmp2 = load %subclass* %tmp1
  store %subclass %tmp2, %subclass* %this
  %.key = getelementptr inbounds %subclass* %this, i32 0, i32 0
  store i32 1, i32* %.key
  ret %subclass* %this
}

define i32 @myclass.calc(%myclass* %this, i32 %x, i32 %y) {
entry:
  %z = alloca i32
  %addtmp = add i32 %x, %y
  store i32 %addtmp, i32* %z
  %z1 = load i32* %z
  ret i32 %z1
}

define %myclass* @myclass.constructor() {
entry:
  %this = alloca %myclass
  %tmp = call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1** null, i32 1) to i32))
  %tmp1 = bitcast i8* %tmp to %myclass*
  %tmp2 = load %myclass* %tmp1
  store %myclass %tmp2, %myclass* %this
  %.key = getelementptr inbounds %myclass* %this, i32 0, i32 0
  store i32 0, i32* %.key
  ret %myclass* %this
}

define i32 @main() {
entry:
  %this = alloca %test
  %tmp = call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1** null, i32 1) to i32))
  %tmp1 = bitcast i8* %tmp to %test*
  %tmp2 = load %test* %tmp1
  store %test %tmp2, %test* %this
  %.key = getelementptr inbounds %test* %this, i32 0, i32 0
  store i32 2, i32* %.key
  %x = alloca i32
  store i32 9, i32* %x
  %y = alloca i32
  store i32 6, i32* %y
  %z = alloca i32
  %obj = alloca %myclass
  %tmp3 = call %myclass* @myclass.constructor()
  %tmp4 = load %myclass* %tmp3
  store %myclass %tmp4, %myclass* %obj
  %cindex = getelementptr inbounds %myclass* %obj, i32 0, i32 0
  %cindex5 = load i32* %cindex
  %fptr = call i64* @lookup(i32 %cindex5, i32 0)
  %myclass.calc = bitcast i64* %fptr to i32 (%myclass*, i32, i32)*
  %x6 = load i32* %x
  %y7 = load i32* %y
  %tmp8 = call i32 %myclass.calc(%myclass* %obj, i32 %x6, i32 %y7)
  store i32 %tmp8, i32* %z
  %z9 = load i32* %z
  %tmp10 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([5 x i8]* @tmp1, i32 0, i32 0), i8* getelementptr inbounds ([3 x i8]* @tmp, i32 0, i32 0), i32 %z9)
  ret i32 0
}
