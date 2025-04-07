
#1.1Calcular expressões

#1
(32/2) + 4
#2
((3^2) *5)+2 
#3
(2+5)*3^2
#4
(39-4^2)/5
#5
(39-4^2)/(5*6)
#6
((39-4^2)/5)*6

#7 
1 - (20/20)*(19/20)*(18/20)*(17/20)*(16/20)
1 - factorial(20)/(factorial(15)*20^5)
1 - prod((20:16)/20)

# Igual o problema dos aniversários
## Melhor forma de fazer, pois reduzimos a dimensão dos números, porém é a mesma forma.;
1 - exp(sum(log((16:20)/20)))
1 - exp(sum(log(16:20)) -5*log(20))


#8
(exp(-2)* 2^3)/factorial(3)
exp(log(-2 +3*log(2) - sum(log(1:3))))

#9
5 + log(8,2)
#10
3 * log(1000,10)
#11
-2*(log(exp(1),4))
#12
log(5,80)

# 2 Vetores
## 2.1 Criar vetores com os seguintes elementos
a <- c(12,14,16,11,12,18,15,12,15) ; a

b <- c(3,7,11,15,19,23) ; b
b <- scan()
b <- seq(from = 3,by = 4, length.out = 6) ;b

c <- c(1,2,4,7,11,16,22,29,37,46,56,67) ; c
c <- 1 + cumsum(0:11) ; c

## 2.2 
### Quantos valores únicos há em a) ?
unique(a)


### Quais os valores em a) maiores que 13 ?
x <- a[a>13] ; x

### Quantos são os valores em a) maiores que 13 ?
y <- a[a<13]; y

### Quantos são os valores em a) maiores que 13 ?
prop <- length(x)/length(a) ; prop

### Qual o resultado em somar o vetor em b) com em c) ?
b ; c
somar <- b+c ; somar

### Quantos valores de a) são divisíveis por 3 ? Quais são eles?
div3 <- a[a%%3 == 0 ]; div3

### Quantas vezes ocorrem cada valor de a)
freq <- table(a); freq

### Qual o valor mais frequente em a)
valorF <- as.numeric(names(which.max(freq))); valorF

## 2.4 Ainda com os vetores criados no exemplo anterior, fornecer comandos que:
# Criando o vetor com as letras até "D"
alfabeto_até_c <- list(a,b,c)

# Inicializando os vetores para armazenar as médias e desvios padrões
vetorMedia <- c()
vetorDesvio <- c()

# Convertendo as letras para seus valores ASCII e calculando a média e desvio padrão
for (i in 1:length(alfabeto_até_c)) {
  # Calculando a média e o desvio padrão
  media <- mean(alfabeto_até_c[[i]])
  desvio <- sd(alfabeto_até_c[[i]])
  
  # Armazenando os resultados
  vetorMedia <- c(vetorMedia, media)
  vetorDesvio <- c(vetorDesvio, desvio)
}
# crie um vetor permutando de posição os elementos do primeiro vetor.
# Vetor original
a <- c(12, 14, 16, 11, 12, 18, 15, 12, 15)

# Permutando (embaralhando) os elementos do vetor 'a'
vetor_permutado <- sample(a)

# Exibindo o vetor permutado
cat("Vetor permutado:", vetor_permutado, "\n")

#crie um vetor amostrando com repetição os elementos do primeiro vetor e formando u vetor de mesmo tamanho.
# Vetor original
a <- c(12, 14, 16, 11, 12, 18, 15, 12, 15)

# Amostrando com repetição para formar um vetor de mesmo tamanho
vetor_amostrado <- sample(a, length(a), replace = TRUE); vetor_amostrado

#repita o item anterior 10 vezes e em cada uma obtenha a média dos vetores criados.
gerar_vetor <- function(tamanho = 5) {
  return(runif(tamanho))  # Vetor com valores aleatórios entre 0 e 1
}

gerar_vetor
vetores <- replicate(10,gerar_vetor()) ; vetores
medias <- apply(vetores, 2, mean); medias
media_total <- mean(medias) ; media_total


fibo <- function(x){
  i <- 0 
  num <- 0
  num2 <- 1
  
  while (i<x-2) {
    aux <- num2
    num2 <- num2 + num
    num <- aux
    i <- i + 1
  }
  return(num2)
  cat(i)
}


fi <- fibo(4)
fi
