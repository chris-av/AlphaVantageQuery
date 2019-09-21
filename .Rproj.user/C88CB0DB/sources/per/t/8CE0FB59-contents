
#' Alpha Vantage Function
#'
#' @param stock_symbol this is the ticker symbol for the object
#' @param frequency this refers to frequency of the data. Valid values include \code{intraday}, \code{daily}, \code{daily_adjusted}, \code{weekly}, \code{weekly_adjusted}, \code{monthly}, \code{monthly_adjusted}, 
#' @param interval in the case that the user desires intraday data, you may select the time intervals for the intraday data frame. Valid values include \code{1min}, \code{5min}, \code{15min}, \code{30min}, \code{60min}
#'
#' @return returns a data frame given the parameters of the API query.
#' @export
#'
#' @examples
#' alpha_vant("GOOGL", frequency = "daily") # a simple example
#' 
#' matrix1 = matrix(
#'   c(
#'     rep(1,3),
#'     rep(1,3),
#'     rep(1,3),
#'     rep(2,3),
#'     rep(2,3)
#'   ), 
#'   nrow = 5, ncol = 3, byrow = TRUE
#' )
#' with(subset(GOOGL_DAILY, GOOGL_DAILY$date > as.Date("2010-01-01")),
#'      {
#'        layout(matrix1)
#'        plot(date, open, type = "l", main = "GOOGL")
#'        lines(date, high, col = "blue", lwd = 0.3)
#'        lines(date, low, col = "blue", lwd = 0.3)
#'        
#'        plot(date, volume, type = "l", main = "GOOGL", col = "#108a4c")
#'      }
#' )
#' 
#'
alpha_vant = function(stock_symbol, frequency, interval = NULL){

  api_key = Sys.getenv("auth_key")  
  tryCatch(
    {
      
      frequency = gsub(" ", "", toupper(frequency))
      data = jsonlite::fromJSON(
        paste0(
          "https://www.alphavantage.co/query?function=TIME_SERIES_", frequency, "&symbol=", stock_symbol,"&interval=", interval, "&outputsize=full&apikey=", api_key
        )
      )
      data = data[[2]]
      if("Error Message" %in% names(data)){
        stop("API call unsuccessful. Ensure that you entered a valid ticker symbol, frequency or interval argument!")
      } else {
        names_vector = names(data[[1]])
        data_frame_names = c("date", gsub('[0-9]. ',"", names(data[[1]])))
        
        
        filler = 1:length(data)
        transformed_data = data.frame(matrix(1, nrow = length(data), ncol = length(data_frame_names)), 
                                      stringsAsFactors = F)
        
        names(transformed_data) = data_frame_names
        transformed_data$date = names(data)
        for(i in 1:length(data)){
          for(j in 1:length(names_vector)){
            
            transformed_data[i, j + 1] = data[[i]][names_vector[j]]
            
          }
        }
        
        transformed_data$date = as.Date(transformed_data$date)
        mod_cols = names(transformed_data)[!(names(transformed_data) %in% c("date", "time"))]
        transformed_data[,mod_cols] = sapply(transformed_data[,mod_cols], as.numeric)
        
        transformed_data = transformed_data[order(transformed_data$date),]
        rownames(transformed_data) = 1:nrow(transformed_data)
        
        assign(paste0(stock_symbol,"_", frequency, interval), transformed_data, envir = .GlobalEnv)
      }
    },
    warning = function(w) {
      message(w)
    },
    error = function(e) {
      message(e)
    }
  )
  
  
  
  
  
}
