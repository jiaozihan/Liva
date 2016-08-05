; ModuleID = 'Liva'

%test = type <{ i32 }>
%calculator = type <{ i32, i32 }>

@tmp = private unnamed_addr constant [3 x i8] c"%d\00"
@tmp.1 = private unnamed_addr constant [18 x i8] c" constructor ok!\0A\00"
@tmp.2 = private unnamed_addr constant [5 x i8] c"%d%s\00"

declare i32 @printf(i8*, ...)

declare i8* @malloc(i32)

define i64* @lookup(i32 %c_index, i32 %f_index) {
entry:
  %tmp = alloca i64**, i32 2
  %tmp1 = alloca i64*, i32 0
  %tmp2 = getelementptr i64**, i64*** %tmp, i32 1
  store i64** %tmp1, i64*** %tmp2
  %tmp3 = alloca i64*
  %tmp4 = getelementptr i64*, i64** %tmp3, i32 0
  store i64* bitcast (i32 (%calculator*, i32, i32)* @calculator.addition to i64*), i64** %tmp4
  %tmp5 = getelementptr i64**, i64*** %tmp, i32 0
  store i64** %tmp3, i64*** %tmp5
  %tmp6 = getelementptr i64**, i64*** %tmp, i32 %c_index
  %tmp7 = load i64**, i64*** %tmp6
  %tmp8 = getelementptr i64*, i64** %tmp7, i32 %f_index
  %tmp9 = load i64*, i64** %tmp8
  ret i64* %tmp9
}

define %test* @test.constructor() {
entry:
  %this = alloca %test
  %tmp = call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i32))
  %tmp1 = bitcast i8* %tmp to %test*
  %tmp2 = load %test, %test* %tmp1
  store %test %tmp2, %test* %this
  %.key = getelementptr inbounds %test, %test* %this, i32 0, i32 0
  store i32 1, i32* %.key
  ret %test* %this
}

define i32 @calculator.addition(%calculator* %this, i32 %x, i32 %y) {
entry:
  %z = alloca i32
  %addtmp = add i32 %x, %y
  store i32 %addtmp, i32* %z
  %z1 = load i32, i32* %z
  ret i32 %z1
}

define %calculator* @calculator.constructor() {
entry:
  %this = alloca %calculator
  %tmp = call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i32))
  %tmp1 = bitcast i8* %tmp to %calculator*
  %tmp2 = load %calculator, %calculator* %tmp1
  store %calculator %tmp2, %calculator* %this
  %.key = getelementptr inbounds %calculator, %calculator* %this, i32 0, i32 0
  store i32 0, i32* %.key
  ret %calculator* %this
}

define i32 @main() {
entry:
  br i1 true, label %then, label %else

then:                                             ; preds = %entry
  %tmp = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @tmp, i32 0, i32 0), i32 1)
  br label %ifcont

else:                                             ; preds = %entry
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %iftmp = phi i32 [ %tmp, %then ], [ 0, %else ]
  %a = alloca i32
  %b = alloca i32
  %c = alloca i32
  store i32 10, i32* %a
  store i32 40, i32* %b
  store i32 4, i32* %c
  %obj = alloca %calculator
  %tmp1 = call %calculator* @calculator.constructor()
  %tmp2 = load %calculator, %calculator* %tmp1
  store %calculator %tmp2, %calculator* %obj
  %cindex = getelementptr inbounds %calculator, %calculator* %obj, i32 0, i32 0
  %cindex3 = load i32, i32* %cindex
  %fptr = call i64* @lookup(i32 %cindex3, i32 0)
  %calculator.addition = bitcast i64* %fptr to i32 (%calculator*, i32, i32)*
  %tmp4 = call i32 %calculator.addition(%calculator* %obj, i32 34, i32 2)
  store i32 %tmp4, i32* %c
  %c5 = load i32, i32* %c
  %tmp6 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @tmp.2, i32 0, i32 0), i32 %c5, i8* getelementptr inbounds ([18 x i8], [18 x i8]* @tmp.1, i32 0, i32 0))
  ret i32 0
}
