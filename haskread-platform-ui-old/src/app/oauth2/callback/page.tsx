'use client';

import { useEffect } from 'react';
import { useSearchParams } from 'next/navigation'
import { useRouter } from 'next/navigation'



const OAuth2Callback: React.FC = () => {
  const router = useRouter();
  const searchParams = useSearchParams()
  
  useEffect(() => {
    const token = searchParams.get('token')

    if (token) {
      localStorage.setItem('jwt_token', token);
      router.push('/');
    } else {
      console.error('No token found in callback');
      router.push('/');
    }
  }, [router,searchParams]);

  return (
    <div>
      <p>Logging you in...</p>
    </div>
  );
};

export default OAuth2Callback;