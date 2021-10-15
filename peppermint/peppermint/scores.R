#----------------------------------------------
# | 2020 데이터 청년 캠퍼스
#
# | 한림대학교 프로그램
# | 산업형 헬스라이프 빅데이터 서비스 개발과정
#
# | 팀: 페퍼민트 
# | 팀원: 노홍민, 전준석, 김민수, 김정현, 김다영
#
# | 파일명: scores.R (scoring functions)
# | 최종수정일: 2020.9.18.
# | 버전: 0.2
#----------------------------------------------

# 스마트폰 함수
smfunc<-function(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o){
  as.numeric(a)+as.numeric(b)+as.numeric(c)+as.numeric(d)+as.numeric(e)
  +as.numeric(f)+as.numeric(g)+as.numeric(h)+as.numeric(i)+as.numeric(j)
  +as.numeric(k)+as.numeric(l)+as.numeric(m)+as.numeric(n)+as.numeric(o)
}

# 우울 함수
woolfunc<-function(a,b,c,d,e,f,g,h,i){
  abs((as.numeric(a)*as.numeric(b))-3.5)/((as.numeric(a)*as.numeric(b))-3.5)*
    (as.numeric(a)+as.numeric(b)+as.numeric(c)+as.numeric(d)+as.numeric(e)
     +as.numeric(f)+as.numeric(g)+as.numeric(h)+as.numeric(i))
}

# 불안 함수
bulfunc <- function(a,b,c,d,e,f,g){
  as.numeric(a)+as.numeric(b)+as.numeric(c)+as.numeric(d)+as.numeric(f)
  +as.numeric(g)
}

# 알콜 함수
alfunc <- function(a,b,c,d,e,f,g,h,i,j){
  as.numeric(a)+as.numeric(b)+as.numeric(c)+as.numeric(d)+as.numeric(e)
  +as.numeric(f)+as.numeric(g)+as.numeric(h)+as.numeric(i)+as.numeric(j)
}

# 도박 함수
dofunc <- function(a,b,c,d,e,f,g,h,i){
  as.numeric(a)+as.numeric(b)+as.numeric(c)+as.numeric(d)+as.numeric(e)
  +as.numeric(f)+as.numeric(g)+as.numeric(h)+as.numeric(i)
}

# 니코틴 함수
nicfunc <- function(a,b,c,d,e,f){
  as.numeric(a)+as.numeric(b)+as.numeric(c)+as.numeric(d)+as.numeric(e)
  +as.numeric(f)
}

#스마트폰 점수
smscore<-function(a,b,c,d,e,f,g,h){
  ((-93.905) + as.numeric(a)*2.959+as.numeric(b)*3.029+as.numeric(c)*3.441
   +as.numeric(d)*2.822+as.numeric(e)*6.18+as.numeric(f)*4.635
   +as.numeric(g)*7.417+as.numeric(h)*0.949)*(-1.3889)+50
}

#도박 점수
doscore<-function(a,b,c,d,e,f,g){
  ((-9.203) + as.numeric(a)*3.615+as.numeric(b)*3.44+as.numeric(c)*3.658
   +as.numeric(d)*4.021+as.numeric(e)*5.755+as.numeric(f)*5.378
   +as.numeric(g)*4.13)*(-2.5316)+66.329
}

#알콜 점수
alscore<-function(a,b,c,d,e,f,g,h,i){
  ((-7.050) + as.numeric(a)*0.386+as.numeric(b)*0.290+as.numeric(c)*0.480
   +as.numeric(d)*0.630+as.numeric(e)*0.471+as.numeric(f)*0.738
   +as.numeric(g)*0.373+as.numeric(h)*0.315+as.numeric(i)*0.495)*(-6.513)+59.8219
}

# 고혈압 점수
bpscore <- function(a,b,c,d,e,f,g,h,i,j,k){
  round(((-50.88 + a*0.237 + b*0.243 + c*0.047 + as.numeric(d)*0.229 + 
            as.numeric(e)*0.426 + f*(-0.084) + as.numeric(g)*(-0.036) +
            h*0.0009 + {i/(j*j)*10000}*(-0.044) + k*0.014)*(-1.515)+54.61),1)
}

# 당뇨 함수
dangscore <- function(a,b,c,d,e,f,g,h,i,j,k,l){round(
  {
    (-7.239 + as.numeric(a)*(-0.034) + b*0.111 + c*0.013 + as.numeric(d)*3.147 +
       e*(-0.004) + {f/(g*g)*10000}*0.033 + as.numeric(h)*0.265 + i*0.003 +
       as.numeric(j)*0.029 + as.numeric(k)*0.035 + l*0.0005)*(-11.1694)+26.9742
  },1)}

#비만 함수
obesscore <- function(a,b,c,d,e,f,g,h,i,j){
  round(((-36.49 + a*0.281 + as.numeric(b)*3.669 + c*0.131 + d*0.202 + e*0.009 +
            f*0.007 + g*0.0019 + h*0.0015 + i*0.006 + j*0.013)*(-1.877)+67.670),1)
}

#간함수
ganscore <- function(a,b,c,d,e,f,g,h,i,j) {round(
  {
    (-5.34 + as.numeric(a)*(-1.127) + b*0.121 + c*0.008 + d*0.004 + e*0.008 +
       as.numeric(f)*0.401 + as.numeric(g)*0.122 + h*0.023 + i*0.009 +
       as.numeric(j)*0.699)*-10.435+47.229
  }, 1)}



#고지혈증 함수
gojiscore <- function(a,b,c,d,e,f,g,h,i,j,k,l,m,n) {round(
  {
    (-13.98 + a*0.064 + b*(-0.055) + c*0.009 + d*0.005 + e*0.009 + as.numeric(f)*0.634 +
       as.numeric(g)*0.045 + h*0.011 + as.numeric(i) + j*0.051 +
       as.numeric(k)*(-0.008) + l*0.004 + {m/(n*n)*10000}*0.038)*(-2.093)+59.234
  }, 1)}

#신장 함수
sinscore <- function(a,b,c,d,e,f,g,h,i) {round(
  {
    (-5.208 + as.numeric(a)*(-0.509) + as.numeric(b)*0.682 + c*0.239 +
       d*(-0.011) + as.numeric(e)*0.366 + as.numeric(f)*0.955 + g*0.445 + 
       {h/(i*i)*10000}*0.023)*(-2.842)+82.943
  }, 1)}