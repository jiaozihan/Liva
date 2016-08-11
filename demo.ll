; ModuleID = 'Liva'

%test = type <{ i32 }>
%myclass = type <{ i32, i32 }>
%subclass = type <{ i32, i32 }>

@tmp = private unnamed_addr constant [3 x i8] c"z=\00"
@tmp1 = private unnamed_addr constant [2 x i8] c"\0A\00"
@tmp2 = private unnamed_addr constant [7 x i8] c"%s%d%s\00"
@tmp3 = private unnamed_addr constant [3 x i8] c"z=\00"
@tmp4 = private unnamed_addr constant [2 x i8] c"\0A\00"
@tmp5 = private unnamed_addr constant [7 x i8] c"%s%d%s\00"
@tmp6 = private unnamed_addr constant [3 x i8] c"z=\00"
@tmp7 = private unnamed_addr constant [2 x i8] c"\0A\00"
@tmp8 = private unnamed_addr constant [7 x i8] c"%s%d%s\00"
@tmp9 = private unnamed_addr constant [3 x i8] c"a[\00"
@tmp10 = private unnamed_addr constant [2 x i8] c"]\00"
@tmp11 = private unnamed_addr constant [4 x i8] c" = \00"
@tmp12 = private unnamed_addr constant [4 x i8] c" , \00"
@tmp13 = private unnamed_addr constant [3 x i8] c"b[\00"
@tmp14 = private unnamed_addr constant [2 x i8] c"]\00"
@tmp15 = private unnamed_addr constant [4 x i8] c" = \00"
@tmp16 = private unnamed_addr constant [2 x i8] c"\0A\00"
@tmp17 = private unnamed_addr constant [25 x i8] c"%s%d%s%s%f%s%s%d%s%s%d%s\00"

declare i32 @printf(i8*, ...)

declare noalias i8* @malloc(i32)

define i64* @lookup(i32 %c_index, i32 %f_index) {
entry:
  %tmp = alloca i64**, i32 3
  %tmp1 = alloca i64*, i32 0
  %tmp2 = getelementptr i64*** %tmp, i32 2
  store i64** %tmp1, i64*** %tmp2
  %tmp3 = alloca i64*
  %tmp4 = getelementptr i64** %tmp3, i32 0
  store i64* bitcast (i32 (%myclass*, i32, i32)* @subclass.add to i64*), i64** %tmp4
  %tmp5 = getelementptr i64*** %tmp, i32 1
  store i64** %tmp3, i64*** %tmp5
  %tmp6 = alloca i64*
  %tmp7 = getelementptr i64** %tmp6, i32 0
  store i64* bitcast (i32 (%myclass*, i32, i32)* @myclass.add to i64*), i64** %tmp7
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

define i32 @subclass.add(%myclass* %this, i32 %x, i32 %y) {
entry:
  %casted = bitcast %myclass* %this to %subclass*
  %z = alloca i32
  %addtmp = add i32 %x, %y
  %addtmp1 = add i32 %addtmp, 1
  store i32 %addtmp1, i32* %z
  %z2 = load i32* %z
  ret i32 %z2
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

define i32 @myclass.add(%myclass* %this, i32 %x, i32 %y) {
entry:
  %z = alloca i32
  %addtmp = add i32 %x, %y
  store i32 %addtmp, i32* %z
  %z1 = load i32* %z
  ret i32 %z1
}

define %myclass* @myclass.constructor.int(i32 %state_tmp) {
entry:
  %this = alloca %myclass
  %tmp = call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1** null, i32 1) to i32))
  %tmp1 = bitcast i8* %tmp to %myclass*
  %tmp2 = load %myclass* %tmp1
  store %myclass %tmp2, %myclass* %this
  %.key = getelementptr inbounds %myclass* %this, i32 0, i32 0
  store i32 0, i32* %.key
  %state = getelementptr inbounds %myclass* %this, i32 0, i32 1
  store i32 %state_tmp, i32* %state
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
  store i32 1, i32* %z
  %obj = alloca %myclass
  %tmp3 = call %myclass* @myclass.constructor.int(i32 1)
  %tmp4 = load %myclass* %tmp3
  store %myclass %tmp4, %myclass* %obj
  %cindex = getelementptr inbounds %myclass* %obj, i32 0, i32 0
  %cindex5 = load i32* %cindex
  %fptr = call i64* @lookup(i32 %cindex5, i32 0)
  %myclass.add = bitcast i64* %fptr to i32 (%myclass*, i32, i32)*
  %x6 = load i32* %x
  %y7 = load i32* %y
  %tmp8 = call i32 %myclass.add(%myclass* %obj, i32 %x6, i32 %y7)
  store i32 %tmp8, i32* %z
  %z9 = load i32* %z
  %tmp10 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([7 x i8]* @tmp2, i32 0, i32 0), i8* getelementptr inbounds ([3 x i8]* @tmp, i32 0, i32 0), i32 %z9, i8* getelementptr inbounds ([2 x i8]* @tmp1, i32 0, i32 0))
  %obj2 = alloca %subclass
  %tmp11 = call %subclass* @subclass.constructor()
  %tmp12 = load %subclass* %tmp11
  store %subclass %tmp12, %subclass* %obj2
  %cindex13 = getelementptr inbounds %subclass* %obj2, i32 0, i32 0
  %cindex14 = load i32* %cindex13
  %fptr15 = call i64* @lookup(i32 %cindex14, i32 0)
  %subclass.add = bitcast i64* %fptr15 to i32 (%myclass*, i32, i32)*
  %tmp16 = bitcast %subclass* %obj2 to %myclass*
  %x17 = load i32* %x
  %y18 = load i32* %y
  %tmp19 = call i32 %subclass.add(%myclass* %tmp16, i32 %x17, i32 %y18)
  store i32 %tmp19, i32* %z
  %z20 = load i32* %z
  %tmp21 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([7 x i8]* @tmp5, i32 0, i32 0), i8* getelementptr inbounds ([3 x i8]* @tmp3, i32 0, i32 0), i32 %z20, i8* getelementptr inbounds ([2 x i8]* @tmp4, i32 0, i32 0))
  %obj5 = alloca %myclass
  %tmp22 = call %myclass* @myclass.constructor.int(i32 5)
  %tmp23 = load %myclass* %tmp22
  store %myclass %tmp23, %myclass* %obj5
  %state = getelementptr inbounds %myclass* %obj5, i32 0, i32 1
  %state24 = load i32* %state
  store i32 %state24, i32* %z
  %z25 = load i32* %z
  %tmp26 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([7 x i8]* @tmp8, i32 0, i32 0), i8* getelementptr inbounds ([3 x i8]* @tmp6, i32 0, i32 0), i32 %z25, i8* getelementptr inbounds ([2 x i8]* @tmp7, i32 0, i32 0))
  %a = alloca double*
  %malloccall = tail call i8* @malloc(i32 mul (i32 add (i32 mul (i32 ptrtoint (double* getelementptr (double* null, i32 1) to i32), i32 10), i32 1), i32 ptrtoint (double* getelementptr (double* null, i32 1) to i32)))
  %tmp27 = bitcast i8* %malloccall to double*
  %tmp28 = bitcast double* %tmp27 to i32*
  store i32 add (i32 mul (i32 ptrtoint (double* getelementptr (double* null, i32 1) to i32), i32 10), i32 1), i32* %tmp28
  br label %array.cond

array.cond:                                       ; preds = %array.init, %entry
  %counter = phi i32 [ 0, %entry ], [ %tmp29, %array.init ]
  %tmp29 = add i32 %counter, 1
  %tmp30 = icmp slt i32 %counter, add (i32 mul (i32 ptrtoint (double* getelementptr (double* null, i32 1) to i32), i32 10), i32 1)
  br i1 %tmp30, label %array.init, label %array.done

array.init:                                       ; preds = %array.cond
  %tmp31 = getelementptr i32* %tmp28, i32 %counter
  store i32 0, i32* %tmp31
  br label %array.cond

array.done:                                       ; preds = %array.cond
  store double* %tmp27, double** %a
  %b = alloca i32*
  %malloccall32 = tail call i8* @malloc(i32 mul (i32 add (i32 mul (i32 ptrtoint (i32* getelementptr (i32* null, i32 1) to i32), i32 10), i32 1), i32 ptrtoint (i32* getelementptr (i32* null, i32 1) to i32)))
  %tmp33 = bitcast i8* %malloccall32 to i32*
  store i32 add (i32 mul (i32 ptrtoint (i32* getelementptr (i32* null, i32 1) to i32), i32 10), i32 1), i32* %tmp33
  br label %array.cond34

array.cond34:                                     ; preds = %array.init35, %array.done
  %counter37 = phi i32 [ 0, %array.done ], [ %tmp38, %array.init35 ]
  %tmp38 = add i32 %counter37, 1
  %tmp39 = icmp slt i32 %counter37, add (i32 mul (i32 ptrtoint (i32* getelementptr (i32* null, i32 1) to i32), i32 10), i32 1)
  br i1 %tmp39, label %array.init35, label %array.done36

array.init35:                                     ; preds = %array.cond34
  %tmp40 = getelementptr i32* %tmp33, i32 %counter37
  store i32 0, i32* %tmp40
  br label %array.cond34

array.done36:                                     ; preds = %array.cond34
  store i32* %tmp33, i32** %b
  %i = alloca i32
  %a41 = load double** %a
  %tmp42 = getelementptr double* %a41, i32 1
  store double 1.000000e+00, double* %tmp42
  %b43 = load i32** %b
  %tmp44 = getelementptr i32* %b43, i32 1
  store i32 1, i32* %tmp44
  store i32 1, i32* %i
  br label %cond

loop:                                             ; preds = %cond
  %i45 = load i32* %i
  %tmp46 = add i32 %i45, 1
  %a47 = load double** %a
  %tmp48 = getelementptr double* %a47, i32 %tmp46
  %i49 = load i32* %i
  %subtmp = sub i32 %i49, 1
  %tmp50 = add i32 %subtmp, 1
  %a51 = load double** %a
  %tmp52 = getelementptr double* %a51, i32 %tmp50
  %tmp53 = load double* %tmp52
  %flt_addtmp = fadd double %tmp53, 1.000000e+00
  store double %flt_addtmp, double* %tmp48
  %i54 = load i32* %i
  %tmp55 = add i32 %i54, 1
  %b56 = load i32** %b
  %tmp57 = getelementptr i32* %b56, i32 %tmp55
  %i58 = load i32* %i
  %subtmp59 = sub i32 %i58, 1
  %tmp60 = add i32 %subtmp59, 1
  %b61 = load i32** %b
  %tmp62 = getelementptr i32* %b61, i32 %tmp60
  %tmp63 = load i32* %tmp62
  %addtmp = add i32 %tmp63, 1
  store i32 %addtmp, i32* %tmp57
  br label %step

step:                                             ; preds = %loop
  %i64 = load i32* %i
  %addtmp65 = add i32 %i64, 1
  store i32 %addtmp65, i32* %i
  br label %cond

cond:                                             ; preds = %step, %array.done36
  %i66 = load i32* %i
  %lesstmp = icmp slt i32 %i66, 10
  br i1 %lesstmp, label %loop, label %afterloop

afterloop:                                        ; preds = %cond
  store i32 0, i32* %i
  br label %cond69

loop67:                                           ; preds = %cond69
  %i71 = load i32* %i
  %i72 = load i32* %i
  %tmp73 = add i32 %i72, 1
  %a74 = load double** %a
  %tmp75 = getelementptr double* %a74, i32 %tmp73
  %tmp76 = load double* %tmp75
  %i77 = load i32* %i
  %i78 = load i32* %i
  %tmp79 = add i32 %i78, 1
  %b80 = load i32** %b
  %tmp81 = getelementptr i32* %b80, i32 %tmp79
  %tmp82 = load i32* %tmp81
  %tmp83 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([25 x i8]* @tmp17, i32 0, i32 0), i8* getelementptr inbounds ([3 x i8]* @tmp9, i32 0, i32 0), i32 %i71, i8* getelementptr inbounds ([2 x i8]* @tmp10, i32 0, i32 0), i8* getelementptr inbounds ([4 x i8]* @tmp11, i32 0, i32 0), double %tmp76, i8* getelementptr inbounds ([4 x i8]* @tmp12, i32 0, i32 0), i8* getelementptr inbounds ([3 x i8]* @tmp13, i32 0, i32 0), i32 %i77, i8* getelementptr inbounds ([2 x i8]* @tmp14, i32 0, i32 0), i8* getelementptr inbounds ([4 x i8]* @tmp15, i32 0, i32 0), i32 %tmp82, i8* getelementptr inbounds ([2 x i8]* @tmp16, i32 0, i32 0))
  br label %step68

step68:                                           ; preds = %loop67
  %i84 = load i32* %i
  %addtmp85 = add i32 %i84, 1
  store i32 %addtmp85, i32* %i
  br label %cond69

cond69:                                           ; preds = %step68, %afterloop
  %i86 = load i32* %i
  %lesstmp87 = icmp slt i32 %i86, 10
  br i1 %lesstmp87, label %loop67, label %afterloop70

afterloop70:                                      ; preds = %cond69
  ret i32 0
}
