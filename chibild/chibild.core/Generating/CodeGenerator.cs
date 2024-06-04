/////////////////////////////////////////////////////////////////////////////////////
//
// chibicc-toolchain - The specialized backend toolchain for chibicc-cil
// Copyright (c) Kouji Matsui(@kozy_kekyo, @kekyo @mastodon.cloud)
//
// Licensed under MIT: https://opensource.org/licenses/MIT
//
/////////////////////////////////////////////////////////////////////////////////////

using chibicc.toolchain.Logging;
using chibicc.toolchain.Tokenizing;
using Mono.Cecil;
using Mono.Cecil.Cil;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace chibild.Generating;

internal sealed partial class CodeGenerator
{
    // Cecil probably does not support multi-threaded access.
    // To speed up chibild, mutable accesses to `ModuleDefinition`
    // (e.g., collection operations such as adding types and members)
    // are delayed until the final stage `ConsumeFinal()`,
    // during which time multithreading speedup is achieved.

    private readonly ILogger logger;
    private readonly ModuleDefinition targetModule;
    private readonly bool produceDebuggingInformation;

    private readonly Queue<Action> delayLookingUpEntries1 = new();
    private readonly Queue<Action> delayLookingUpEntries2 = new();
    private readonly Queue<Action<Dictionary<string, Document>, bool>> delayDebuggingInsertionEntries = new();

    private bool caughtError;
    private int placeholderIndex;

    public CodeGenerator(
        ILogger logger,
        ModuleDefinition targetModule,
        bool produceDebuggingInformation)
    {
        this.logger = logger;
        this.targetModule = targetModule;
        this.produceDebuggingInformation = produceDebuggingInformation;
    }
    
    //////////////////////////////////////////////////////////////

    private void OutputError(Token token, string message)
    {
        this.caughtError = true;
        this.logger.Error(
            $"{token.RelativePath}:{token.Line + 1}:{token.StartColumn + 1}: {message}");
    }

    private void OutputWarning(Token token, string message)
    {
        this.logger.Warning(
            $"{token.RelativePath}:{token.Line + 1}:{token.StartColumn + 1}: {message}");
    }

    private void OutputTrace(Token token, string message)
    {
        this.logger.Trace(
            $"{token.RelativePath}:{token.Line + 1}:{token.StartColumn + 1}: {message}");
    }

    //////////////////////////////////////////////////////////////

    private void ConsumeFragment(
        ObjectInputFragment currentFragment,
        InputFragment[] inputFragments)
    {
        var context = new LookupContext(
            this.targetModule,
            currentFragment,
            inputFragments);

#if DEBUG
        foreach (var variable in currentFragment.GlobalVariables)
        {
            this.ConsumeGlobalVariable(context, variable);
        }
        foreach (var constant in currentFragment.GlobalConstants)
        {
            this.ConsumeGlobalConstant(context, constant);
        }
        foreach (var function in currentFragment.Functions)
        {
            this.ConsumeFunction(context, function);
        }
        foreach (var initializer in currentFragment.Initializers)
        {
            this.ConsumeInitializer(context, initializer);
        }
        foreach (var enumeration in currentFragment.Enumerations)
        {
            this.ConsumeEnumeration(context, enumeration);
        }
        foreach (var structure in currentFragment.Structures)
        {
            this.ConsumeStructure(context, structure);
        }
#else
        Parallel.Invoke(
            () =>
            {
                foreach (var variable in currentFragment.GlobalVariables)
                {
                    this.ConsumeGlobalVariable(context, variable);
                }
            },
            () =>
            {
                foreach (var constant in currentFragment.GlobalConstants)
                {
                    this.ConsumeGlobalConstant(context, constant);
                }
            },
            () =>
            {
                foreach (var function in currentFragment.Functions)
                {
                    this.ConsumeFunction(context, function);
                }
            },
            () =>
            {
                foreach (var initializer in currentFragment.Initializers)
                {
                    this.ConsumeInitializer(context, initializer);
                }
            },
            () =>
            {
                foreach (var enumeration in currentFragment.Enumerations)
                {
                    this.ConsumeEnumeration(context, enumeration);
                }
            },
            () =>
            {
                foreach (var structure in currentFragment.Structures)
                {
                    this.ConsumeStructure(context, structure);
                }
            });
#endif
    }

    public void Clear()
    {
        this.delayLookingUpEntries1.Clear();
        this.delayLookingUpEntries2.Clear();
        this.delayDebuggingInsertionEntries.Clear();
        this.placeholderIndex = 0;
        this.caughtError = false;
    }

    private void ConsumeArchivedObject(
        InputFragment[] inputFragments,
        bool isLocationOriginSource)
    {
        bool found;
        do
        {
            found = false;
#if DEBUG
            foreach (var currentFragment in inputFragments)
            {
                switch (currentFragment.LoadObjectIfRequired(
                    this.logger,
                    isLocationOriginSource))
                {
                    case LoadObjectResults.Loaded:
                        if (currentFragment is ArchivedObjectInputFragment afif)
                        {
                            // This consume may cause another `InputFragment` to become `Required` state.
                            // Therefore, it is a `do-while` loop to check again.
                            found = true;
                            this.ConsumeFragment(afif, inputFragments);
                        }
                        break;
                    case LoadObjectResults.Ignored:
                        break;
                    default:
                        this.caughtError = true;
                        break;
                }
            }
#else
            Parallel.ForEach(inputFragments,
                new() { MaxDegreeOfParallelism = Math.Max(1, Environment.ProcessorCount - 1) },
                currentFragment =>
                {
                    switch (currentFragment.LoadObjectIfRequired(
                        this.logger,
                        isLocationOriginSource))
                    {
                        case LoadObjectResults.Loaded:
                            if (currentFragment is ArchivedObjectInputFragment afif)
                            {
                                // This consume may cause another `InputFragment` to become `Required` state.
                                // Therefore, it is a `do-while` loop to check again.
                                found = true;
                                this.ConsumeFragment(afif, inputFragments);
                            }
                            break;
                        case LoadObjectResults.Ignored:
                            break;
                        default:
                            this.caughtError = true;
                            break;
                    }
                });
#endif
        }
        // Repeat if at least one `ArchivedObjectInputFragment` is processed and there are no errors.
        while (found && !this.caughtError);
    }

    public bool ConsumeInputs(
        InputFragment[] inputFragments,
        bool isLocationOriginSource)
    {
        ////////////////////////////////////
        // Step 1. Consume all objects.

#if DEBUG
        foreach (var currentFragment in inputFragments.
            OfType<ObjectInputFragment>().
            Where(fragment => fragment is not ArchivedObjectInputFragment))
        {
            this.ConsumeFragment(currentFragment, inputFragments);
        }
#else
        Parallel.ForEach(inputFragments,
            new() { MaxDegreeOfParallelism = Math.Max(1, Environment.ProcessorCount - 1) },
            currentFragment =>
            {
                if (currentFragment is ObjectInputFragment ofif &&
                    ofif is not ArchivedObjectInputFragment)
                {
                    this.ConsumeFragment(ofif, inputFragments);
                }
            });
#endif
        if (this.caughtError)
        {
            return false;
        }

        ////////////////////////////////////
        // Step 2. Consume scheduled object referring in archives.

        this.ConsumeArchivedObject(
            inputFragments,
            isLocationOriginSource);

        return !this.caughtError;
    }
}
