; ModuleID = 'Liva'

%gcd = type <{ i32 }>

@tmp = private unnamed_addr constant [5 x i8] c"gcd=\00"
@tmp1 = private unnamed_addr constant [5 x i8] c"%s%d\00"

declare i32 @printf(i8*, ...)

declare i8* @malloc(i32)

define i64* @lookup(i32 %c_index, i32 %f_index) {
entry:
  %tmp = alloca i64**
  %tmp1 = alloca i64*, i32 0
  %tmp2 = getelementptr i64*** %tmp, i32 0
  store i64** %tmp1, i64*** %tmp2
  ret i64* null
}

define %gcd* @gcd.constructor() {
entry:
  %this = alloca %gcd
  %tmp = call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1** null, i32 1) to i32))
  %tmp1 = bitcast i8* %tmp to %gcd*
  %tmp2 = load %gcd* %tmp1
  store %gcd %tmp2, %gcd* %this
  %.key = getelementptr inbounds %gcd* %this, i32 0, i32 0
  store i32 0, i32* %.key
  ret %gcd* %this
}

define i32 @main() {
entry:
  %this = alloca %gcd
  %tmp = call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1** null, i32 1) to i32))
  %tmp1 = bitcast i8* %tmp to %gcd*
  %tmp2 = load %gcd* %tmp1
  store %gcd %tmp2, %gcd* %this
  %.key = getelementptr inbounds %gcd* %this, i32 0, i32 0
  store i32 0, i32* %.key
  %x = alloca i32
  %y = alloca i32
  %z = alloca i32
  store i32 66, i32* %x
  store i32 98, i32* %y
  br label %cond

loop:                                             ; preds = %cond
  %x3 = load i32* %x
  %y4 = load i32* %y
  %sgttmp = icmp sgt i32 %x3, %y4
  br i1 %sgttmp, label %then, label %else

then:                                             ; preds = %loop
  %x5 = load i32* %x
  %y6 = load i32* %y
  %subtmp = sub i32 %x5, %y6
  store i32 %subtmp, i32* %x
  br label %ifcont

else:                                             ; preds = %loop
  %y7 = load i32* %y
  %x8 = load i32* %x
  %subtmp9 = sub i32 %y7, %x8
  store i32 %subtmp9, i32* %y
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %iftmp = phi i32 [ %subtmp, %then ], [ %subtmp9, %else ]
  br label %step

step:                                             ; preds = %ifcont
  br label %cond

cond:                                             ; preds = %step, %entry
  %x10 = load i32* %x
  %y11 = load i32* %y
  %neqtmp = icmp ne i32 %x10, %y11
  br i1 %neqtmp, label %loop, label %afterloop

afterloop:                                        ; preds = %cond
  %x12 = load i32* %x
  %tmp13 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([5 x i8]* @tmp1, i32 0, i32 0), i8* getelementptr inbounds ([5 x i8]* @tmp, i32 0, i32 0), i32 %x12)
  ret i32 0
}
