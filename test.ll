; ModuleID = 'Liva'

%test = type <{ i32 }>
%my_calculator = type <{ i32, i32 }>
%calculator = type <{ i32, i32 }>

@tmp = private unnamed_addr constant [3 x i8] c"%d\00"
@tmp1 = private unnamed_addr constant [3 x i8] c"%d\00"
@tmp2 = private unnamed_addr constant [3 x i8] c"\0A\0A\00"
@tmp3 = private unnamed_addr constant [18 x i8] c" constructor ok!\0A\00"
@tmp4 = private unnamed_addr constant [7 x i8] c"%s%d%s\00"

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
  store i64* bitcast (i32 (%my_calculator*, i32, i32)* @my_calculator.addition to i64*), i64** %tmp4
  %tmp5 = getelementptr i64*** %tmp, i32 1
  store i64** %tmp3, i64*** %tmp5
  %tmp6 = alloca i64*
  %tmp7 = getelementptr i64** %tmp6, i32 0
  store i64* bitcast (i32 (%calculator*, i32, i32)* @calculator.addition to i64*), i64** %tmp7
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

define i32 @my_calculator.addition(%my_calculator* %this, i32 %x, i32 %y) {
entry:
  %z = alloca i32
  %addtmp = add i32 %x, %y
  store i32 %addtmp, i32* %z
  %z1 = load i32* %z
  ret i32 %z1
}

define %my_calculator* @my_calculator.constructor() {
entry:
  %this = alloca %my_calculator
  %tmp = call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1** null, i32 1) to i32))
  %tmp1 = bitcast i8* %tmp to %my_calculator*
  %tmp2 = load %my_calculator* %tmp1
  store %my_calculator %tmp2, %my_calculator* %this
  %.key = getelementptr inbounds %my_calculator* %this, i32 0, i32 0
  store i32 1, i32* %.key
  ret %my_calculator* %this
}

define i32 @calculator.addition(%calculator* %this, i32 %x, i32 %y) {
entry:
  %z = alloca i32
  %addtmp = add i32 %x, %y
  store i32 %addtmp, i32* %z
  %z1 = load i32* %z
  ret i32 %z1
}

define %calculator* @calculator.constructor() {
entry:
  %this = alloca %calculator
  %tmp = call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1** null, i32 1) to i32))
  %tmp1 = bitcast i8* %tmp to %calculator*
  %tmp2 = load %calculator* %tmp1
  store %calculator %tmp2, %calculator* %this
  %.key = getelementptr inbounds %calculator* %this, i32 0, i32 0
  store i32 0, i32* %.key
  ret %calculator* %this
}

define i32 @main() {
entry:
  br i1 true, label %then, label %else

then:                                             ; preds = %entry
  %tmp = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([3 x i8]* @tmp, i32 0, i32 0), i32 1)
  br label %ifcont

else:                                             ; preds = %entry
  %tmp1 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([3 x i8]* @tmp1, i32 0, i32 0), i32 2)
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %iftmp = phi i32 [ %tmp, %then ], [ %tmp1, %else ]
  %a = alloca i32
  %b = alloca i32
  %c = alloca i32
  store i32 10, i32* %a
  store i32 40, i32* %b
  store i32 4, i32* %c
  %obj = alloca %my_calculator
  %tmp2 = call %my_calculator* @my_calculator.constructor()
  %tmp3 = load %my_calculator* %tmp2
  store %my_calculator %tmp3, %my_calculator* %obj
  %cindex = getelementptr inbounds %my_calculator* %obj, i32 0, i32 0
  %cindex4 = load i32* %cindex
  %fptr = call i64* @lookup(i32 %cindex4, i32 0)
  %my_calculator.addition = bitcast i64* %fptr to i32 (%my_calculator*, i32, i32)*
  %tmp5 = call i32 %my_calculator.addition(%my_calculator* %obj, i32 34, i32 2)
  store i32 %tmp5, i32* %c
  %c6 = load i32* %c
  %tmp7 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([7 x i8]* @tmp4, i32 0, i32 0), i8* getelementptr inbounds ([3 x i8]* @tmp2, i32 0, i32 0), i32 %c6, i8* getelementptr inbounds ([18 x i8]* @tmp3, i32 0, i32 0))
  ret i32 0
}
