#light

module FSharp.MathTools.Optimization

/// Represents a position used during the optimization process. This structure performs lazy computation
/// of the function, gradient and Hessian information as follows: the user initializes a point by passing
/// the evaluations functions to the constructors. Only when an optimization routine actually accesses either
/// the function value, gradient or Hessian, will it be evaluated and remembered.
type Point =
    class end
    with
        /// Constructor for points that need to support only function, gradient and Hessian evaluation.
        new : p:vector * feval:(vector -> float) * geval:(vector -> vector) *
              heval:(vector -> matrix) -> Point
        /// Constructor for points that need to support only function and gradient evaluation.
        new : p:vector * feval:(vector -> float) * geval:(vector -> vector) -> Point
        /// Constructor for points that need to support only function evaluation.
        new : p:vector * feval:(vector -> float) -> Point
        /// The function value at the point's position.
        member f : float
        /// The gradient vector at the point's position.
        member g : vector
        /// The Hessian matrix at the point's position.
        member h : matrix
        /// The point's position.
        member p : vector


/// Any line search routine must implement this interface so multivariate optimization procedures can use
/// the line search as a subroutine.
type ILineSearch =
    /// Finds an appropriate step size (where appropriate is determined by the type of line search implemented).
    abstract FindStep : (vector -> float) -> (vector -> vector) -> Point -> vector -> float * Point

/// Represents a backtracking line search algorithm (Nocedal & Wright, p37). Starting with a step size equal to InitialStep,
/// the procedure will try to find a value that decreases the function by at least: C x StepSize x (Gradient'.Direction). If
/// the current step size under consideration doesn't decrease the function sufficiently, it will multiply the step size by
/// the Rescale factor. This method will also rescale the step size if the function value under consideration is NaN.
type BackTrackLineSearch =
    class
        /// The initial step size to try. Default = 1.0
        val mutable InitialStep : float
        /// The rescaling factor when the current stepsize does not imply a sufficient decrease. Default = 0.9
        val mutable Rescale : float
        /// The sufficient decrease condition, must be an element of (0,1). Default = 0.75
        val mutable C : float
    end
    with
        interface ILineSearch
        /// Default constructor.
        new : unit -> BackTrackLineSearch
        
/// Represents a line search algorithm that ends when it finds a point that satisfies the strong Wolfe conditions (Nocedal & Wright, p60).
/// Starting with a step size equal to InitialStep, the procedure will try to find an interval that certainly contains a point that satisfies
/// the strong Wolfe conditions. It then shortens the interval to zoom in onto a point that satisfies the strong Wolfe conditions.
type WolfeLineSearch =
  class 
    /// The initial step size to try.
    val mutable InitialStep: float
    /// The amount by which to increase the initial step if it is too short.
    val mutable Rescale: float
    /// The maximum number of iterations that we allow to find an interval that contians a point that satisfies the strong Wolfe conditions.
    val mutable MaxIter: int
    /// The sufficient decrease condition, must be an element of (0,C2).
    val mutable C1: float
    /// The curvature condition, must be an element of (0,1).
    val mutable C2: float
    /// The callback routine for every step of the interval searching subroutine.
    val mutable Phase1Callback: float * Point -> unit
    /// The callback routine for every step of the zoom subroutine. This procedure passes the low and high (stepsize * Point) of the current best interval.
    val mutable Phase2Callback: float * Point -> float * Point -> unit
  end
  with
    interface ILineSearch
    /// Creates a line search object with default parameters: InitialStep = 1.0, Rescale = 2.0, C1 = 1.0e-4,
    /// C2 = 0.9, MaxIter = 100.
    new : unit -> WolfeLineSearch
    /// Creates a line search object with specific parameter settings. InitialStep = is, Rescale = rs, C1 = c1, C2 = c2,
    /// MaxIter = maxiter, Phase1Callback = p1cb, Phase2Callback = p2cb.
    new : is:float * rs:float * c1:float * c2:float * maxiter:int * p1cb:(float * Point -> unit) * p2cb:(float * Point -> float * Point -> unit) -> WolfeLineSearch

  end

/// Gradient descent optimization algorithm: the gradient descent algorithm performs a step in the negative gradient direction
/// at every step. The stepsize will be selected by some line search routine (Nocedal & Wright, p19).
type GradientDescent =
  class
    /// The maximal number of steps we allow the gradient descent to take.
    val mutable MaxIter: int
    /// The stop condition: if the gradient norm of our current point is below this number, we end the gradient descent procedure.
    val mutable Stop: float
    /// The line search routine which we want to use; this member defaults to the Wolfe line search.
    val mutable LineSearch: ILineSearch
    /// The callback routine which will be called at every iteration. The first argument is the iteration number, the second is the
    /// current best point.
    val mutable Callback: int -> Point -> unit
  end
  with
    /// Creates a gradient descent object with default parameters: MaxIter = 100, Stop = 1.0e-6, LineSearch = new WolfeLineSearch().
    new : unit -> GradientDescent
    /// Runs the gradient descent minimization procedure. This routine will stop when either the norm is smaller than the Stop variable,
    /// when the maximum number of iterations has been taken or when the function value at some point becomes NaN.
    member Run : (vector -> float) -> (vector -> vector) -> vector -> Point * int
  end


/// Conjugate gradient optimization algorithm (Nocedal & Wright, p121). We currently implement a Polak-Ribiere conjugate gradient.
type ConjugateGradient =
  class
    /// The maximal number of steps we allow the conjugate gradient to take.
    val mutable MaxIter: int
    /// The stop condition: if the gradient norm of our current point is below this number, we end the conjugate gradient procedure.
    val mutable Stop: float
    /// The line search routine which we want to use; this defaults to the Wolfe line search.
    val mutable LineSearch: ILineSearch
    /// The callback routine which will be called at every iteration. The first argument is the iteration number, the second is the
    /// current best point.
    val mutable Callback: int -> Point -> unit
  end
  with
    /// Creates a gradient descent object with default parameters: MaxIter = 100, Stop = 1.0e-6, LineSearch = new WolfeLineSearch().
    /// We also change the line search to use C1 = 0.05 and C2 = 0.1.
    new : unit -> ConjugateGradient
    /// Runs the conjugate gradient minimization procedure. This routine will stop when either the norm is smaller than the Stop variable,
    /// when the maximum number of iterations has been taken, when we are still decreasing the function value or when the function value
    /// at some point becomes NaN.
    member Run : (vector -> float) -> (vector -> vector) -> vector -> Point * int
  end