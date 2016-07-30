; ModuleID = 'Liva'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt_string = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt_string.1 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt_float = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt.2 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt_string.3 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt_string.4 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt_float.5 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@str = private unnamed_addr constant [6 x i8] c"false\00"
@fmt.6 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt_string.7 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt_string.8 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt_float.9 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt.10 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt_string.11 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt_string.12 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt_float.13 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt.14 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt_string.15 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt_string.16 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt_float.17 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt.18 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt_string.19 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt_string.20 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt_float.21 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@str.22 = private unnamed_addr constant [13 x i8] c"Hello World!\00"
@fmt.23 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt_string.24 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt_string.25 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt_float.26 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt.27 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt_string.28 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt_string.29 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt_float.30 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@str.31 = private unnamed_addr constant [6 x i8] c"false\00"

declare i32 @printf(...)

define i32 @main() {
entry:
  %printf = call i32 (...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.6, i32 0, i32 0), i32 100)
  %printf1 = call i32 (...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt_string.15, i32 0, i32 0), i8* getelementptr inbounds ([13 x i8], [13 x i8]* @str.22, i32 0, i32 0))
  %printf2 = call i32 (...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt_string.25, i32 0, i32 0), i8* getelementptr inbounds ([6 x i8], [6 x i8]* @str.31, i32 0, i32 0))
  ret i32 0
}
