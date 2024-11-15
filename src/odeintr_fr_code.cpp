// Copyright Timothy H. Keitt 2015
// See license for odeintr package

// [[Rcpp::depends(odeintr)]]

#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::depends(BH)]]
#include "boost/numeric/odeint.hpp"
namespace odeint = boost::numeric::odeint;

;

namespace odeintr
{
  static const std::size_t N = 1;

  typedef std::vector<double> state_type;
  
  static state_type state(N);
  
  typedef odeint::runge_kutta_cash_karp54<state_type> stepper_type;
  
  static auto stepper = stepper_type();
  
  typedef std::vector<double> vec_type;
  static std::vector<vec_type> rec_x(N);
  static vec_type rec_t;
  
  double Fmax, N0, h, P;;
  
  #include "utils.h"
  
  static void
  sys(const state_type x, state_type &dxdt, const double t)
  {
    dxdt[0] = -Fmax * pow(x[0],h) / (pow(N0,h) + pow(x[0],h)) * P;;
  }

  static void
  obs(const state_type x, const double t)
  {
    for (int i = 0; i != N; ++i)
      rec_x[i].push_back(x[i]);
    rec_t.push_back(t);
  }
  
}; // namespace odeintr

static void
reserve(odeintr::vec_type::size_type n)
{
  odeintr::rec_t.reserve(n);
  for (auto &i : odeintr::rec_x) i.reserve(n);
}

// [[Rcpp::export]]
Rcpp::List FR_get_output()
{
  Rcpp::List out;
  out("Time") = Rcpp::wrap(odeintr::rec_t);
  for (int i = 0; i != odeintr::N; ++i)
  {
    auto cnam = std::string("X") + std::to_string(i + 1);
    out(cnam) = Rcpp::wrap(odeintr::rec_x[i]);
  }
  out.attr("class") = "data.frame";
  int rows_out = odeintr::rec_t.size();
  auto rn = Rcpp::IntegerVector::create(NA_INTEGER, -rows_out);
  out.attr("row.names") = rn;
  return out;
};

// [[Rcpp::export]]
void FR_set_state(Rcpp::NumericVector new_state)
{
  if (new_state.size() != odeintr::N)
    Rcpp::stop("Invalid initial state");
  std::copy(new_state.begin(),
            new_state.end(),
            odeintr::state.begin());
}

// [[Rcpp::export]]
std::vector<double>
FR_get_state()
{
  return odeintr::state;
}

// [[Rcpp::export]]
void FR_reset_observer()
{
  for (auto &i : odeintr::rec_x) i.resize(0);
  odeintr::rec_t.resize(0);  
}

// [[Rcpp::export]]
Rcpp::List FR_adap(Rcpp::NumericVector init,
                             double duration,
                             double step_size = 1.0,
                             double start = 0.0)
{
  FR_set_state(init);
  FR_reset_observer(); reserve(duration / step_size);
  odeint::integrate_adaptive(odeintr::stepper, odeintr::sys, odeintr::state,
                             start, start + duration, step_size,
                             odeintr::obs);
  return FR_get_output();
}

// [[Rcpp::export]]
Rcpp::List FR_at(Rcpp::NumericVector init,
                           std::vector<double> times,
                           double step_size = 1.0,
                           double start = 0.0)
{
  FR_set_state(init);
  FR_reset_observer(); reserve(times.size());
  odeint::integrate_const(odeintr::stepper, odeintr::sys, odeintr::state,
                          start, times[0], step_size);
  odeint::integrate_times(odeintr::stepper, odeintr::sys, odeintr::state,
                          times.begin(), times.end(), step_size, odeintr::obs);
  return FR_get_output();
}

// [[Rcpp::export]]
Rcpp::List
FR_continue_at(std::vector<double> times, double step_size = 1.0)
{
  double start = odeintr::rec_t.back();
  FR_reset_observer(); reserve(odeintr::rec_t.size() + times.size());
  odeint::integrate_const(odeintr::stepper, odeintr::sys, odeintr::state,
                          start, times[0], step_size);
  odeint::integrate_times(odeintr::stepper, odeintr::sys, odeintr::state,
                          times.begin(), times.end(), step_size, odeintr::obs);
  return FR_get_output();
}

// [[Rcpp::export]]
Rcpp::List FR(Rcpp::NumericVector init,
                       double duration,
                       double step_size = 1.0,
                       double start = 0.0)
{
  FR_set_state(init);
  FR_reset_observer(); reserve(duration / step_size);
  odeint::integrate_const(odeintr::stepper, odeintr::sys, odeintr::state,
                          start, start + duration, step_size,
                          odeintr::obs);
  return FR_get_output();
}

// [[Rcpp::export]]
std::vector<double>
FR_no_record(Rcpp::NumericVector init,
                       double duration,
                       double step_size = 1.0,
                       double start = 0.0)
{
  FR_set_state(init);
  odeint::integrate_adaptive(odeintr::stepper, odeintr::sys, odeintr::state,
                             start, start + duration, step_size);
  return FR_get_state();
}

// [[Rcpp::export]]
void FR_set_params(double Fmax, double N0, double h, double P)
{ 
  odeintr::Fmax = Fmax;
odeintr::N0 = N0;
odeintr::h = h;
odeintr::P = P;
}
// [[Rcpp::export]]
Rcpp::List FR_get_params()
{
  Rcpp::List out;
  out["Fmax"] = odeintr::Fmax;
out["N0"] = odeintr::N0;
out["h"] = odeintr::h;
out["P"] = odeintr::P;
  return out;
}
;



