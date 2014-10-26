library(ggplot2)
library(grid)
library(zoo)

loader <- function(duration=10, clients.per.test=clients.per.sec*duration,
                   clients.per.sec=clients.per.test/duration,
                   response.min=0.5, response.max=1.5)
{
  values <- head(seq(0, duration, by=1/clients.per.sec),-1)
  start.times <- trunc(values)
  response.times <- runif(n=length(start.times), min=response.min, max=response.max)

  clients <- data.frame(start.time=start.times, response.time=response.times, y=values)
  p <- ggplot(clients, aes(x=start.time, y=y))
  (p <- p + geom_segment(aes(xend=start.time+response.time, yend=y),
                         arrow=arrow(type="closed", length = unit(0.125, "inches"))))
  (p <- p + scale_x_continuous(breaks=0:duration))
  (p <- p + theme_bw())
  (p <- p + xlab("Time (s)"))
  (p <- p + ylab("Client ID"))
#  (p <- p + theme(axis.text.y=element_blank()))
  # Hide y gridlines
  (p <-p + theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank()))
  return(p)
}

maintain.load <- function(duration=10, clients.from=0, clients.to=5,
                          response.min=0.5, response.max=1.5,
                          delta=0.1
                          )
{
  # First, we need to use from/to to figure out how many clients
  # at the different times
  # We distribute them between t=0 and t=(duration-1)
  Tmax <- duration-1
  times <- 0:Tmax
  m <- (clients.to - clients.from)/Tmax
  b <- clients.from
  y <- trunc(m*times + b)
  clients.at.time <- zoo(trunc(y))
  new.clients <- c(clients.at.time[1], clients.at.time - lag(clients.at.time, -1))
  clients <- data.frame(time=times, new.clients=new.clients)

  # Horrible, horrible use of global vars

  assign("client.id", 0, envir = .GlobalEnv)
  assign("df.clients", data.frame(), envir = .GlobalEnv)

  # Generate a data frame with the times
  gen.frames <- function(client) {
    start.time <- client[1]
    new.clients <- client[2]
    if(new.clients>0) {
      for(i in c(1:new.clients)) {
        assign("client.id", client.id + 1, envir = .GlobalEnv)
        Nmax <- ceiling((duration - start.time) / (response.min+delta))
        if(Nmax>0) {
          response.durations <- runif(Nmax, min=response.min, max=response.max)
          start.times <- vector()
          start.times[1] <- start.time
          if(Nmax>1) {
            for(i in 2:Nmax) {
              start.times[i] <- start.time[1] + sum(response.durations[1:i-1]) + (i-1)*delta
            }
          }
          end.times <- start.times + response.durations
          df.times <- data.frame(start.time=start.times,
                                 end.time=end.times,
                                 y=rep(client.id, Nmax))
          assign("df.clients",
                 rbind(df.clients, df.times),
                 envir = .GlobalEnv)
        }
      }
    }
  }
  apply(clients, 1, gen.frames)

  # Only take ones that start before time is up
  df.clients <- df.clients[df.clients$start.time<duration,]
  p <- ggplot(df.clients, aes(x=start.time, y=y))
  (p <- p + geom_segment(aes(xend=end.time, yend=y), arrow=arrow(type="closed",
                                                                 length = unit(0.125, "inches"))))
  (p <- p + scale_x_continuous(breaks=0:duration))
  (p <- p + theme_bw())
  (p <- p + xlab("Time (s)"))
  (p <- p + ylab("Client ID"))
  #(p <- p + theme(axis.text.y=element_blank()))
  # Hide y gridlines
  (p <-p + theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank()))
  return(p)
}

p.cps <- loader(duration=20, clients.per.sec=5) +
         ggtitle("Clients per second\n5 clients\n20s duration")
p.cpt <- loader(duration=20, clients.per.test=100) +
         ggtitle("Clients per test\n100 clients\n20s duration")
p.ml <- maintain.load(duration=20, clients.to=50) +
         ggtitle("Maintain client load\nFrom 0 to 50 clients\n20s duration")

ggsave("clients-per-second.png", p.cps)
ggsave("clients-per-test.png", p.cpt)
ggsave("maintain-client-load.png", p.ml)
