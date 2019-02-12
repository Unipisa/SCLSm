#light

module FSharp.MathTools.Optimization

open Microsoft.FSharp.Math


/// Represents a position used during the optimization process. This structure performs lazy computation
/// of the function, gradient and Hessian information as follows: the user initializes a point by passing
/// the evaluations functions to the constructors. Only when an optimization routine actually accesses either
/// the function value, gradient or Hessian, will it be evaluated and remembered.
type Point = class
    val _p : vector                            //  Represents the position
    val _f : Lazy<float>                       //  Represents the function value
    val _g : Lazy<vector>                      //  Represents the gradient
    val _h : Lazy<matrix>                      //  Represents the Hessian
    /// Constructor for points that need to support only function evaluation.
    new(p, feval) = {_p = p; _f = lazy (feval p); _g = lazy (failwith "No gradient evaluation function given."); _h = lazy (failwith "No Hessian evaluation function given.")}
    /// Constructor for points that need to support only function and gradient evaluation.
    new(p, feval, geval) = {_p = p; _f = lazy (feval p); _g = lazy (geval p); _h = lazy (failwith "No Hessian evaluation function given.")}
    /// Constructor for points that need to support only function, gradient and Hessian evaluation.
    new(p, feval, geval, heval) = {_p = p; _f = lazy (feval p); _g = lazy (geval p); _h = lazy (heval p)}
    /// The point's position.
    member this.p
        with get () = this._p
    /// The function value at the point's position.
    member this.f
        with get () = Lazy.force this._f
    /// The gradient vector at the point's position.
    member this.g
        with get () = Lazy.force this._g
    /// The Hessian matrix at the point's position.
    member this.h
        with get () = Lazy.force this._h
end


/// Any line search routine must implement this interface so multivariate optimization procedures can use
/// the line search as a subroutine.
type ILineSearch =
    /// Finds an appropriate step size (where appropriate is determined by the type of line search implemented).
    abstract FindStep : (vector -> float) -> (vector -> vector) -> Point -> vector -> float * Point


/// Represents a backtracking line search algorithm (Nocedal & Wright, p37). Starting with a step size equal to InitialStep,
/// the procedure will try to find a value that decreases the function by at least: C x StepSize x (Gradient'.Direction). If
/// the current step size under consideration doesn't decrease the function sufficiently, it will multiply the step size by
/// the Rescale factor. This method will also rescale the step size if the function value under consideration is NaN.
type BackTrackLineSearch = class
    /// The initial step size to try.
    val mutable InitialStep : float
    /// The rescaling factor when the current stepsize does not imply a sufficient decrease.
    val mutable Rescale : float
    /// The sufficient decrease condition, must be an element of (0,1).
    val mutable C : float
    
    /// Creates a backtracking line search object with default parameters.
    new() = { InitialStep = 1.0;
              Rescale = 0.9;
              C = 0.75 }
              
    interface ILineSearch with
        /// Returns a step size which implies sufficient decrease.
        member ls.FindStep f g (x0: Point) (p: vector) =
                    
            // This variable remembers the projection of the gradient in the direction we are searching.
            let phi'0 = (Vector.transpose x0.g) * p
            
            /// Returns a step size which implies sufficient decrease. The method may return a zero step size if accuracy prohibits
            /// finding a small enough step size or when we are in a local minimum for the search direction.
            let rec ls_inner alpha_i =
                let xnew = new Point(x0.p + alpha_i $* p, f, g)
                
                if (xnew.f > x0.f + ls.C * alpha_i * phi'0) || System.Double.IsNaN(xnew.f) then
                    if alpha_i <> 0.0 then
                        ls_inner (alpha_i * ls.Rescale)
                    else
                        alpha_i, xnew
                else
                    alpha_i, xnew
            
            ls_inner ls.InitialStep
    end
end




/// Represents a line search algorithm that ends when it finds a point that satisfies the strong Wolfe conditions (Nocedal & Wright, p60).
/// Starting with a step size equal to InitialStep, the procedure will try to find an interval that certainly contains a point that satisfies
/// the strong Wolfe conditions. It then shortens the interval to zoom in onto a point that satisfies the strong Wolfe conditions.
type WolfeLineSearch = class
    /// The initial step size to try.
    val mutable InitialStep : float
    /// The amount by which to increase the initial step if it is too short.
    val mutable Rescale : float
    /// The maximum number of iterations that we allow to find an interval that contians a point that satisfies the strong Wolfe conditions.
    val mutable MaxIter : int
    /// The sufficient decrease condition, must be an element of (0,C2).
    val mutable C1 : float
    /// The curvature condition, must be an element of (0,1).
    val mutable C2 : float
    /// The callback routine for every step of the interval searching subroutine.
    val mutable Phase1Callback : (float * Point) -> unit
    /// The callback routine for every step of the zoom subroutine. This procedure passes the low and high (stepsize * Point) of the current best interval.
    val mutable Phase2Callback : (float * Point) -> (float * Point) -> unit
    
    /// Creates a line search object with default parameters: InitialStep = 1.0, Rescale = 2.0, C1 = 1.0e-4,
    /// C2 = 0.9, MaxIter = 100.
    new() = { InitialStep = 1.0;
              Rescale = 2.0;
              C1 = 1.0e-4;
              C2 = 0.9;
              MaxIter = 100;
              Phase1Callback = fun _ -> () ;
              Phase2Callback = fun _ _ -> () }
    
    /// Creates a line search object with specific parameter settings.
    new(is, rs, c1, c2, maxiter, p1cb, p2cb) = { InitialStep = is;
                                                 Rescale = rs;
                                                 C1 = c1;
                                                 C2 = c2;
                                                 MaxIter = maxiter;
                                                 Phase1Callback = p1cb ;
                                                 Phase2Callback = p2cb }
    
    interface ILineSearch with
        /// Returns a step  which implies a point that satisfies the strong Wolfe conditions.
        member ls.FindStep f g (x0: Point) (p: vector) =
            // This method will find an appropriate step along the direction p such that the strong
            // Wolfe conditions are satisfied. The method will perform a one dimensional minimization
            // of the function phi(alpha) = f(x + alpha * p).T * p.
            
            /// Helper routine for quadratic interpolation given two points, their function values and
            /// the derivative at one of the points. This procedure also performs safeguarding 
            /// by not allowing the interpolant to be within 0.1 of the given points.
            let qinterpol x1 x2 y1 y2 dx1 =
                let ret = x1 - (dx1 * ((x2 - x1)**2.0)) / (2.0 * (y2 - y1 - dx1*(x2 - x1)))
                let sg = 0.1 * abs(x1 - x2)
                if (x1 - sg) < ret && ret < x1 then x1 - sg
                elif (x2 - sg) < ret && ret < x2 then x2 - sg
                elif (x1 + sg) > ret && ret >= x1 then x1 + sg
                elif (x2 + sg) > ret && ret >= x2 then x2 + sg
                elif (y2 - y1 - dx1*x1 = 0.0) then x1 + (x2 - x1)/2.0
                else ret
        
            let alpha_ip = 0.0
            let alpha_i = ls.InitialStep
        
            // This is the one dimensional gradient when alpha is 0. We require that it is a descent direction.
            let phi'0 = (Vector.transpose x0.g) * p
            if phi'0 >= 0.0 then failwith "The linesearch requires p to be a descent direction."

            // The Phase 2 subroutine which zooms in onto a Strong Wolfe point in an interval that certainly contains a Strong Wolfe point.
            let zoom alpha_lo alpha_hi (x_lo: Point) (x_hi: Point) =
                
                let rec zoom_inner i alpha_lo alpha_hi (x_lo: Point) (x_hi: Point) =
                    
                    ls.Phase2Callback (alpha_lo, x_lo) (alpha_hi, x_hi)
                    
                    //DEBUG printf "    zoom: lo = %e, lof = %e\n          hi = %e, hif = %e\n" alpha_lo x_lo.f alpha_hi x_hi.f
                    if i > ls.MaxIter || alpha_lo = alpha_hi then
                        (alpha_lo, x_lo)
                    else
                        let alpha_j = qinterpol alpha_lo alpha_hi x_lo.f x_hi.f ((Vector.transpose x_lo.g) * p)
                        let x_j = new Point(x0.p + alpha_j $* p, f, g)
                        if (x_j.f > x0.f + ls.C1 * alpha_j * phi'0) || (x_j.f >= x_lo.f) then
                            zoom_inner (i+1) alpha_lo alpha_j x_lo x_j
                        else
                            let dphij = (Vector.transpose x_j.g) * p
                            if abs(dphij) <= -ls.C2 * phi'0 then
                                (alpha_j, x_j)
                            elif (dphij * (alpha_hi - alpha_lo)) >= 0.0 then
                                zoom_inner (i+1) alpha_j alpha_lo x_j x_lo
                            else
                                zoom_inner (i+1) alpha_j alpha_hi x_j x_hi
                
                // Starts zooming (Phase 2).
                zoom_inner 1 alpha_lo alpha_hi x_lo x_hi
        
            // The Phase 1 subroutine which finds an interval that certainly contains a Strong Wolfe point.
            let rec ls_inner i alpha_ip alpha_i (x: Point) =
            
                let xnew = new Point(x0.p + alpha_i $* p, f, g)
                
                ls.Phase1Callback (alpha_i, xnew)
                
                //DEBUG printf "    ls_inner: alpha = %e; fun = %e\n" alpha_i xnew.f
                if i > ls.MaxIter then
                    (alpha_i, x)
                else
                    if (xnew.f > x0.f + ls.C1 * alpha_i * phi'0) || (xnew.f >= x.f) then
                        zoom alpha_ip alpha_i x xnew
                    else
                        let dphi_i = (Vector.transpose xnew.g)*p
                        if abs(dphi_i) <= -ls.C2*phi'0 then
                            (alpha_i, xnew)
                        elif dphi_i >= 0.0 then
                            zoom alpha_i alpha_ip xnew x
                        else
                            ls_inner (i+1) alpha_i (ls.Rescale*alpha_i) xnew
            
            // Starts interval finding (Phase 1).       
            ls_inner 1 alpha_ip alpha_i x0
    end
end



/// Gradient descent optimization algorithm: the gradient descent algorithm performs a step in the negative gradient direction
/// at every step. The stepsize will be selected by some line search routine (Nocedal & Wright, p19).
type GradientDescent = class
    /// The maximal number of steps we allow the gradient descent to take.
    val mutable MaxIter : int
    /// The stop condition: if the gradient norm of our current point is below this number, we end the gradient descent procedure.
    val mutable Stop : float
    /// The line search routine which we want to use; this member defaults to the Wolfe line search.
    val mutable LineSearch : ILineSearch
    /// The callback routine which will be called at every iteration. The first argument is the iteration number, the second is the
    /// current best point.
    val mutable Callback : int -> Point -> unit
    
    /// Creates a gradient descent object with default parameters: MaxIter = 100, Stop = 1.0e-6, LineSearch = new WolfeLineSearch().
    new() = { MaxIter = 100;
              Stop = 1.0e-6;
              Callback = (fun _ _ -> ());
              LineSearch = (new WolfeLineSearch() :> ILineSearch) }
    
    /// Runs the gradient descent minimization procedure. This routine will stop when either the norm is smaller than the Stop variable,
    /// when the maximum number of iterations has been taken or when the function value at some point becomes NaN.
    member gd.Run f g (x0:vector) =
        let norm x = sqrt((Vector.transpose x) * x)     // TODO remove when F# matrix library is fixed.
        
        let rec gd_inner i (x: Point) =
            gd.Callback i x
                
            if norm(x.g) <= gd.Stop || i >= gd.MaxIter || System.Double.IsNaN(x.f) then
                (x, i)
            else
                let (alpha, xnew) = gd.LineSearch.FindStep f g x (-x.g)
                gd_inner (i+1) xnew
        
        gd_inner 0 (new Point(x0, f, g))
end



/// Conjugate gradient optimization algorithm (Nocedal & Wright, p121). We currently implement a Polak-Ribiere conjugate gradient.
type ConjugateGradient = class
    /// The maximal number of steps we allow the conjugate gradient to take.
    val mutable MaxIter : int
    /// The stop condition: if the gradient norm of our current point is below this number, we end the conjugate gradient procedure.
    val mutable Stop : float
    /// The line search routine which we want to use; this defaults to the Wolfe line search.
    val mutable LineSearch : ILineSearch
    /// The callback routine which will be called at every iteration. The first argument is the iteration number, the second is the
    /// current best point.
    val mutable Callback : int -> Point -> unit
    
    /// Creates a conjugate gradient object with default parameters: MaxIter = 100, Stop = 1.0e-6, LineSearch = new WolfeLineSearch().
    /// We also change the line search to use C1 = 0.05 and C2 = 0.1.
    new() = { MaxIter = 100;
              Stop = 1.0e-6;
              Callback = (fun _ _ -> ());
              LineSearch = (new WolfeLineSearch(1.0, 2.0, 0.05, 0.1, 100, (fun _ -> ()), (fun _ _ -> ())) :> ILineSearch) }
    
    /// Runs the conjugate gradient minimization procedure. This routine will stop when either the norm is smaller than the Stop variable,
    /// when the maximum number of iterations has been taken, when we are still decreasing the function value or when the function value
    /// at some point becomes NaN.
    member gd.Run f g (x0:vector) =
        let mutable x = new Point(x0, f, g)                     // The latest point found by CG.
        let mutable p = -x.g                                    // The initial direction in which CG is going to step.
        let mutable k = 1
        let norm v = sqrt((Vector.transpose v) * v)             // TODO remove
        let mutable delta = System.Double.PositiveInfinity      // The amount of change since the last iteration.
        
        while (norm p) > gd.Stop && k <= gd.MaxIter && delta > 0.0 && not(System.Double.IsNaN(x.f)) do
            let (alpha, newx) = gd.LineSearch.FindStep f g x p                                  // Find a minimizer along the direction p
            let beta = max ((newx.g.Transpose * (newx.g - x.g)) / (x.g.Transpose * x.g)) 0.0
            p <- -newx.g + beta $* p                                                            // Find the next search direction
            
            // Sometimes we don't have a descent direction. Doing a gradient descent step can often help.
            if p.Transpose * newx.g > 0.0 then
                p <- -newx.g
            
            delta <- abs (newx.f - x.f)
            x <- newx                                                                           // Prepare next iteration
            gd.Callback k x                                                                     // Call the debug function
            k <- k+1
            
        // Return the minimum
        (x, k-1)
end