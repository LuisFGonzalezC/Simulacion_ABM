# Preparatorio ------------------------------------------------------------
pacman::p_load(ABM, tidyverse) # Carga el paquete ABM: define simulaciones basadas en agentes.
set.seed(1) # Fijamos la semilla para que los resultados sean reproducibles.


# Parámetros del modelo ---------------------------------------------------
N <- 5000 #Número de hogares (Cada uno es un "agente")
Tmax <- 36 #Número de meses (ticks) a simular
transfer_amount <- 600 #Monto nominal mensual del programa ($600)
delay_prob_base <- 0.3 #Probabilidad de retraso del pago (30%). Credibilidad inicial = 1 - (.3) = .7
lambda_cred <- 0.3 #Qué tan rápido "aprende" la credibilidad (promedio móv)

# Crear simulación y estado inicial ---------------------------------------
#En ABM, el "estado" principal del agente será un string "A" (asiste) o "N" (no asiste)
#Además, guardamos atributos por agente:
# - Theta: umbral individuos de decisión
# - cred: credibilidad percibida de cobrar a tiempo (entre 0 y 1).

sim<- Simulation$new(N) #Crea una simulación con N agentes (IDS 1. .N ya existen interna)
seedA<- 10 #Cantidad de hogares que ya comienzan asistiendo en t=0
for (i in 1:N) { #Recorremos todos los agentes para fijar su estado inicial
  theta <- rnorm(1, mean = 300, sd = 80) #Es el umbral heterogéneo ~ Normal (300, 80)
  cred0 <- 1 - delay_prob_base #Credibilidad inicial (ej. 0.7)
  state0 <- if(i <=seedA) "A" else "N" #Primeros "seed" agentes arrancan en "A",
  sim$setState(i, list(state0, theta = theta, cred = cred0))
}
#setState (indice, lista):
# -El primer elemento de la lista es el ESTADO ("A"/"N")
# -El resto son atributos nombrados que viajan con el agente
#Fija el estado del agente i como el primer elemento de la lista (A/N) y guarda atributos nombrados (theta, cred, etc.)

# Loggers (contadores automáticos) ----------------------------------------
#newCounter (nombre, desde): crea un contador que, en cada tick, cuenta cuántos
sim$addLogger(newCounter("A", "A")) #Cuantos asisten
sim$addLogger(newCounter("N", "N")) #Cuántos no asisten

# Handler mensual (la "dinámica" que corre cada mes) ----------------------
tick_handler <- function(time, sim, agent) {   # Recorremos TODOS los agentes y actualizamos su estado de este mes
  for (i in 1:N) {
    ai <- getAgent(sim, i)           # Obtiene el agente i
    st <- getState(ai)               # st[[1]] es "A" o "N"; luego atributos
    
    # Beneficio esperado este mes: monto * credibilidad percibida
    benefit   <- transfer_amount * st$cred
    
    # Regla de decisión mínima:
    #   Asiste si el beneficio esperado supera su umbral theta
    new_state <- if (benefit >= st$theta) "A" else "N"
    
    # Si asiste, “experimento” de pago a tiempo (Bernoulli con p = 1 - delay_prob_base)
    paid_on_time <- (new_state == "A") && (runif(1) > delay_prob_base)
    
    # Actualizar credibilidad (promedio móvil)
    cred_new <- (1 - lambda_cred) * st$cred + lambda_cred * as.numeric(paid_on_time)
    
    # Guardamos nuevo estado + atributos (con el theta que no cambia)
    setState(ai, list(new_state, theta = st$theta, cred = cred_new))
  }
  
  # Re-agendar el mismo handler para el mes siguiente, hasta Tmax
  # Usamos 'agent' (puntero del propio Simulation) como contenedor del evento recurrente
  if (time < Tmax) schedule(agent, newEvent(time + 1, tick_handler))
}

# Ejecutar la simulación --------------------------------------------------

# Programamos el primer “tick” en t = 0 sobre la entidad 'sim$get' (el agente "Simulación")
schedule(sim$get, newEvent(0, tick_handler))

# sim$run(0:Tmax) avanza la simulación y devuelve un data.frame con:
#   - columna temporal (times)
#   - columnas de cada logger (A, N)
res <- sim$run(0:Tmax)     # data.frame con columnas: time, A, N

# Creamos una columna con la proporción que asiste (A/N total)
res$attend <- res$A / N

# Graficamos --------------------------------------------------------------
p <- ggplot(res, aes(x = times, y = attend)) +
  geom_area(fill = "yellow2", alpha = 0.12) +
  geom_line(size = 1.4, color = "orange", lineend = "round") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "CCT (umbral + credibilidad)", x = "Mes", y = "% que asiste") +
  theme_minimal(base_size = 14)

p

