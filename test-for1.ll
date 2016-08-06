; ModuleID = 'Liva'

%test = type <{ i32 }>

@tmp = private unnamed_addr constant [3 x i8] c"%d\00"
@tmp1 = private unnamed_addr constant [3 x i8] c"%d\00"

declare i32 @printf(i8*, ...)

declare i8* @malloc(i32)

define i64* @lookup(i32 %c_index, i32 %f_index) {
entry:
  %tmp = alloca i64**
  %tmp1 = alloca i64*
  %tmp2 = getelementptr i64** %tmp1, i32 0
  store i64* bitcast (i32 (%test*)* @test.yan to i64*), i64** %tmp2
  %tmp3 = getelementptr i64*** %tmp, i32 0
  store i64** %tmp1, i64*** %tmp3
  %tmp4 = getelementptr i64*** %tmp, i32 %c_index
  %tmp5 = load i64*** %tmp4
  %tmp6 = getelementptr i64** %tmp5, i32 %f_index
  %tmp7 = load i64** %tmp6
  ret i64* %tmp7
}

define i32 @test.yan(%test* %this) {
entry:
  %j = alloca i32
  store i32 3, i32* %j
  br label %cond

loop:                                             ; preds = %cond
  %j1 = load i32* %j
  %tmp = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([3 x i8]* @tmp, i32 0, i32 0), i32 %j1)
  %j2 = load i32* %j
  %subtmp = sub i32 %j2, 1
  store i32 %subtmp, i32* %j
  br label %step

step:                                             ; preds = %loop
  br label %cond

cond:                                             ; preds = %step, %entry
  %j3 = load i32* %j
  %sgttmp = icmp sgt i32 %j3, 2
  br i1 %sgttmp, label %loop, label %afterloop

afterloop:                                        ; preds = %cond
  ret i32 0
}

define %test* @test.constructor() {
entry:
  %this = alloca %test
  %tmp = call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1** null, i32 1) to i32))
  %tmp1 = bitcast i8* %tmp to %test*
  %tmp2 = load %test* %tmp1
  store %test %tmp2, %test* %this
  %.key = getelementptr inbounds %test* %this, i32 0, i32 0
  store i32 0, i32* %.key
  ret %test* %this
}

define i32 @main() {
entry:
  %i = alloca i32
  store i32 0, i32* %i
  br label %cond

loop:                                             ; preds = %cond
  %i1 = load i32* %i
  %tmp = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([3 x i8]* @tmp1, i32 0, i32 0), i32 %i1)
  br label %step

step:                                             ; preds = %loop
  %i2 = load i32* %i
  %addtmp = add i32 %i2, 1
  store i32 %addtmp, i32* %i
  br label %cond

cond:                                             ; preds = %step, %entry
  %i3 = load i32* %i
  %lesstmp = icmp slt i32 %i3, 2
  br i1 %lesstmp, label %loop, label %afterloop

afterloop:                                        ; preds = %cond
  ret i32 0
}
