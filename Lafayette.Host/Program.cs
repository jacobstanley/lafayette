using Lafayette;
using Orleans;
using System;

namespace Lafayette.Host
{
    /// <summary>
    /// Orleans test silo host
    /// </summary>
    public class Program
    {
        static void Main(string[] args)
        {
            // The Orleans silo environment is initialized in its own app domain in order to more
            // closely emulate the distributed situation, when the client and the server cannot
            // pass data via shared memory.
            AppDomain hostDomain = AppDomain.CreateDomain("OrleansHost", null, new AppDomainSetup
            {
                AppDomainInitializer = InitSilo,
                AppDomainInitializerArguments = args,
            });

            OrleansClient.Initialize("DevTestClientConfiguration.xml");

            var func = GrainFunc.from<Examples.Occupation, int>(0);

            var developer = Examples.Occupation.NewDeveloper(10, Examples.mklist<double>(5, 10, 15));
            Console.WriteLine(func.Invoke(developer).Result);

            // shouldn't work
            var func2 = GrainFunc.from<Examples.Occupation, double>(0);
            func2.Invoke(developer).Wait();

            Console.WriteLine("Orleans Silo is running.\nPress Enter to terminate...");
            Console.ReadLine();

            hostDomain.DoCallBack(ShutdownSilo);
        }

        static void InitSilo(string[] args)
        {
            hostWrapper = new OrleansHostWrapper(args);

            if (!hostWrapper.Run())
            {
                Console.Error.WriteLine("Failed to initialize Orleans silo");
            }
        }

        static void ShutdownSilo()
        {
            if (hostWrapper != null)
            {
                hostWrapper.Dispose();
                GC.SuppressFinalize(hostWrapper);
            }
        }

        private static OrleansHostWrapper hostWrapper;
    }
}
