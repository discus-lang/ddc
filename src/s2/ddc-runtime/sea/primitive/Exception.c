#pragma once
#include <setjmp.h>
#include <stdio.h>


// ----------------------------------------------------------------------------
// Frame to restore the current context.
struct _DDCPrimExceptionFrame;
struct _DDCPrimExceptionFrame
{       // Buffer to store the setjmp context.
        jmp_buf* jmp_buf;

        // What the top of the GC shadow stack was when we constructed
        // the setjmp context. This stack gets modified by LLVM on entry
        // and exit from functions, but it won't get a chance to pop
        // the stack when we call longjmp.
        struct DDCPrimCollectStackEntry* llvm_root_chain;
};
typedef struct _DDCPrimExceptionFrame DDCPrimExceptionFrame;


// Global variable to store the current exception context.
//   If we have nested exceptions then references to the deeper frames
//   are stored in the stack frames of the nested ddcPrimTry functions.
DDCPrimExceptionFrame*  ddcPrimExceptionFrame = 0;


// Global variable used by ddcPrimThrow to send a value describing
//   the exception to the handler invocation in ddcPrimTry;
Obj*    ddcPrimExceptionValue = 0;


// ----------------------------------------------------------------------------
// Import functions from Apply.dcs module that allow us to run
// the computation and handler thunks.
extern Obj* ddcRunThunk(Obj* thunk);
extern Obj* ddcApply1(Obj* thunk, Obj* arg);


// ddcPrimTry
//      (comp:    S eff1 a)
//      (handler: Exception -> S eff2 a)
//      : S ((eff1 - Throw) + eff2) a
//
// Try execting the given computation,
//  if it throws an exception then run the given handler.
//
// We can't express the type directly as a signature, as we want to cut
// off the Throw effect. This is why 'try' needs its own typing rule.
//
Obj* ddcPrimExceptionTry(Obj* thunk, Obj* handler)
{
        // Allocate a new exception frame and set that to the current one.
        DDCPrimExceptionFrame* oldFrame
         = ddcPrimExceptionFrame;

        ddcPrimExceptionFrame
         = malloc(sizeof(DDCPrimExceptionFrame));

        ddcPrimExceptionFrame->jmp_buf
         = malloc(sizeof(jmp_buf));

        ddcPrimExceptionFrame->llvm_root_chain
         = llvm_gc_root_chain;

        // Call setjmp to save the context.
        if (!setjmp(*ddcPrimExceptionFrame->jmp_buf))
        {       // Run the computation.
                Obj* result = ddcRunThunk(thunk);

                // If we reach this point then the computation finished
                // successfully without throwing an exception,
                // so we can deallocate the exception frame.
                free(ddcPrimExceptionFrame->jmp_buf);
                free(ddcPrimExceptionFrame);
                ddcPrimExceptionFrame = oldFrame;

                return result;
        }
        else
        {       // If we reach this point then the computation threw an
                // exception. We now need to deallocate the current context
                // frame and restore the old one before calling the handler,
                // as the handler itself may throw another exception.
                free(ddcPrimExceptionFrame->jmp_buf);
                free(ddcPrimExceptionFrame);
                llvm_gc_root_chain    = ddcPrimExceptionFrame->llvm_root_chain;
                ddcPrimExceptionFrame = oldFrame;

                // Now it's safe to call the handler.
                return ddcRunThunk(ddcApply1(handler, ddcPrimExceptionValue));
        }
}


// ddcPrimThrow (ex: Exception): S Throw Unit
void ddcPrimExceptionThrow(Obj* value)
{       // If there is no context frame then there is no handler to return to.
        // We also don't have any way of printing out the thrown exception
        // value, so just produce this uninformative error.
        // For compiled Discus Source programs the top-level handler should
        // be installed automatically by the 'main' function hook.
        if (ddcPrimExceptionFrame == 0)
        {       printf("ddc-runtime.ddcPrimThrow: no top-level handler.\n");
                abort();
        }

        // Use the global to send the thrown value to the handler.
        ddcPrimExceptionValue = value;

        // Bye.
        longjmp(*ddcPrimExceptionFrame->jmp_buf, 1);

        // We never arrive here.
        abort();
}

