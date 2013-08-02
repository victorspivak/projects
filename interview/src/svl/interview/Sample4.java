package svl.interview;

public class Sample4
{
    public static void main (String[] args)
    {
        Sample4   o1  = new Sample4();
        Object  o2  = makeIt();

        if (o1.getClass().getName().equals(o2.getClass().getName()))
        {
            Sample4   o3  = (Sample4) o2;//Would it always work?
        }
    }






    






















































    static Object makeIt ()
    {
        throw new RuntimeException();
    }
}
