; ModuleID = 'Liva'

@tmp = private unnamed_addr constant [3 x i8] c"%d\00"
@tmp.1 = private unnamed_addr constant [3 x i8] c"%d\00"
@tmp.2 = private unnamed_addr constant [3 x i8] c"%d\00"
@tmp.3 = private unnamed_addr constant [3 x i8] c"%d\00"
@tmp.4 = private unnamed_addr constant [3 x i8] c"%d\00"

declare i32 @printf(i8*, ...)

define i32 @main() {
entry:
  %tmp = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @tmp, i32 0, i32 0), i32 100)
  br i1 true, label %then, label %else

then:                                             ; preds = %entry
  %tmp1 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @tmp.1, i32 0, i32 0), i32 42)
  br label %ifcont

else:                                             ; preds = %entry
  %tmp2 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @tmp.2, i32 0, i32 0), i32 8)
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %iftmp = phi i32 [ %tmp1, %then ], [ %tmp2, %else ]
  %tmp3 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @tmp.3, i32 0, i32 0), i32 17)
  %a = alloca i32
  store i32 1000000, i32* %a
  %a4 = load i32, i32* %a
  %tmp5 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @tmp.4, i32 0, i32 0), i32 %a4)
  ret i32 0
}
