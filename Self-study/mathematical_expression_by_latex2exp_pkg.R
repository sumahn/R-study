# 수식을 입력하는 방법
# latex2exp <- r 패키지

# 데이터 만들기
x <- seq(1, 10, by=0.1)
plot_dat <- data.frame(x = x,
                       y = 10 * sin(x)^2 + x^2)

with(plot_dat, plot(x = x, 
                    y= y,
                    type="l",
                    main="f(x) = 10*sin(x)^2+x^2"))

# 원래 방식
with(plot_dat, 
     plot(x = x, 
          y= y,
          type="l",
          main=expression(f(x) == 10*sin(x)^2+x^2)))


# latex2exp pkg 사용
library(latex2exp)
# expression 넣고 싶은 곳에 
# TeX()
with(plot_dat, plot(x = x, 
                    y= y,
                    type="l",
                    main=TeX("$f(x) = 10sin^{2}(x)+x^2$")))

ggplot(data = plot_dat,
       aes(x=x, y=y)) +
    geom_line() + theme_bw() +
    labs(title = TeX("$f(x) = 10sin^{2}(x)+x^2$")) +
    annotate("text", x=2.5, y=75,
             label = TeX("$f(x)=\\sum_{i=1}^{n}(x_i - \\mu)^2$"))
     